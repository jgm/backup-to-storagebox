#!/bin/bash
set -e

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 config.json"
  exit 1
fi

CONFIG="$1"

config () {
  jq -r "$1" < "$CONFIG"
}

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

# Populate exclude file:
truncate -s 0 "$EXCLUDEFILE"
for file in "$EXCLUDES"; do
  echo "$file" >> "$EXCLUDEFILE"
done

restic () {
  echo "$PASSWORD" | /opt/homebrew/bin/restic -r "$REPO" "$@"
}

echo "-------------------------------- $(date)"

err_exit () {
        echo "Finished with errors."
        MESSAGE="Backup finished with errors. Check log for details."
        TITLE="backup"
        osascript -e "display notification \"$MESSAGE\" with title \"$TITLE\""
	exit 1
}

trap 'err_exit' ERR SIGHUP SIGQUIT SIGINT SIGTERM

# First unlock any stale locks left over from failed backups...
restic unlock

# Now do the backup...
restic backup --host "$HOSTNAME" \
  --exclude-caches --exclude-larger-than "$MAXFILESIZE" \
  --exclude-file "$EXCLUDEFILE" \
  --verbose \
  "$FILES"

# Get a diff with the last snapshot
DIFFARGS=$(restic snapshots latest --json | jq -r '.[0] | (.parent,.id)')
echo "Getting diff of $DIFFARGS"
restic diff $DIFFARGS

# Purge if we haven't done it for a week:
find "$PURGESENTINAL" -mtime -7 || \
  restic forget --prune \
    --keep-daily "$DAILIES" \
    --keep-weekly "$WEEKLIES" \
    --keep-monthly "$MONTHLIES" \
    --keep-yearly "$YEARLIES"  && touch "$PURGESENTINAL"

echo "Success."
