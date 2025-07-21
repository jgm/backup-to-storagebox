#!/bin/sh
set -e

if [ "$#" -eq 1 ]; then
  CONFIG="$1" 
else
  if [ -z "$BACKUP_TO_STORAGEBOX_CONFIG" ]; then
    echo "Usage: backup-to-storagebox config.json"
    exit 1
  else
    CONFIG="$BACKUP_TO_STORAGEBOX_CONFIG"
  fi
fi

config () {
  jq -r "$1" < "$CONFIG"
}

UNAME=$(uname)

# Don't go to sleep while we're backing up.
if [ "$UNAME" = "Darwin" ]; then
  CAFFEINATE="caffeinate -i"
fi

HOSTNAME=$(config '.hostname')
if [ "$HOSTNAME" = "null" ]; then
  HOSTNAME="$(hostname)"
fi
FILES=$(config '.files.[]')
REPO=$(config '.repository')
PURGESENTINAL="$TMPDIR/backup.$REPO.purged"
EXCLUDEFILE="$TMPDIR/backup.$REPO.exclude"
PASSWORD=$(config '.password')
EXCLUDES=$(config '.excludes.[]')
HOURLIES=$(config '.hourlies')
DAILIES=$(config '.dailies')
WEEKLIES=$(config '.weeklies')
MONTHLIES=$(config '.monthlies')
YEARLIES=$(config '.yearlies')
MAXFILESIZE=$(config '.maxFileSize')
RESTICPATH=$(which restic || echo '/opt/homebrew/bin/restic')

# Populate exclude file:
truncate -s 0 "$EXCLUDEFILE"
for file in "$EXCLUDES"; do
  echo "$file" >> "$EXCLUDEFILE"
done

restic () {
  echo "$PASSWORD" | $CAFFEINATE $RESTICPATH -r "$REPO" "$@"
}

echo "-------------------------------- $(date)"

err_exit () {
        echo "Finished with errors."
        MESSAGE="Backup finished with errors. Check log for details."
        TITLE="backup"
        if [ "$UNAME" = "Darwin" ]; then
          osascript -e "display notification \"$MESSAGE\" with title \"$TITLE\""
        fi
        # echo "$MESSAGE" | mail -s "$TITLE" $(whoami)
	exit 1
}

trap '[ $? -eq 0 ] && exit 0 || err_exit' EXIT

# First unlock any stale locks left over from failed backups...
restic unlock

# Now do the backup...
restic backup --host "$HOSTNAME" \
  --exclude-caches --exclude-larger-than "$MAXFILESIZE" \
  --exclude-file "$EXCLUDEFILE" \
  --verbose \
  $FILES

# Get a diff with the last snapshot
DIFFARGS=$(restic snapshots latest --json | jq -r '.[0] | (.parent,.id)')
echo "Getting diff of $DIFFARGS"
restic diff $DIFFARGS

# Purge if we haven't done it for a day:
find "$PURGESENTINAL" -mtime -1 || \
  restic forget --prune \
    --keep-daily "$DAILIES" \
    --keep-weekly "$WEEKLIES" \
    --keep-monthly "$MONTHLIES" \
    --keep-yearly "$YEARLIES"  && touch "$PURGESENTINAL"

echo "Success."
