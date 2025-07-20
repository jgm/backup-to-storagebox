# backup-to-storagebox

A backup utility that uses [restic](https://restic.net/) to
backup files to a remote repository (designed for Hetzner Storage
Box or similar services).  A single JSON configuration file contains all
the information needed for a backup. When the backup is complete, a
macOS notification is issued, including the path of the (temporary) log
file.

The backup script can be used on its own, but if it is to be run
in a macOS LaunchAgent, it needs to have "full disk access"
granted, and this can only be done for a binary, not a script.
So this repository includes a Makefile that will compile the
script into binary via C.

## Prerequisites

- [restic](https://restic.net/) installed and available in PATH.

## Installation

Clone the repository, then `make install` to install the binary
to `~/.local/bin`.

## Usage

Create a JSON configuration file with your backup settings:

```json
{
  "hostname": "your-hostname",
  "repository": "sftp:user@your-storage-box.com:reponame",
  "password": "your-restic-repository-password",
  "excludes": [
    "*.tmp",
    "node_modules",
    ".git"
  ],
  "files": [
    "/path/to/backup1",
    "/path/to/backup2"
  ],
  "maxFileSize": "2G",
  "hourlies": 2,
  "dailies": 7,
  "weeklies": 4,
  "monthlies": 12,
  "yearlies": 50
}
```

Because this contains a password, make sure it is only readable by
the user (`chmod 600`).

`"hostname"` can be omitted, in which case the host will be set dynamically
to the value returned by `hostname`. But it is a good idea to set it
manually if your environment will modify the hostname (e.g. via a VPN).

Then run the backup:

```sh
backup-to-storagebox /path/to/your/config.json
```

## Configuration

The JSON configuration file supports the following fields:

- `repository`: The restic repository URL (e.g., SFTP, S3, local path)
- `password`: Password for the restic repository
- `excludes`: Array of patterns to exclude from backup
- `files`: Array of files and directories to backup

## Backup Process

The tool performs the following steps:

1. Unlocks the repository (in case of stale locks)
2. Creates a backup with the following options:
   - Excludes cache directories (`--exclude-caches`)
   - Applies custom exclude patterns
   - Excludes files over a certain size (e.g. 2G)
3. Compares the new snapshot with the previous one using `restic diff`
   and puts the differences in the log.
4. Every 24 hours: prunes old snapshots according to retention
   policy, with a specified number of hourlies, dailies,
   weeklies, monthlies, and yearlies.
5. Issues a macOS notification if there were errors.

## Logging

All backup operations are logged to stdout and stderr.

## Scheduling using a LaunchAgent

Add a LaunchAgent `.plist` file in `~/Library/LaunchAgents`. Here is
an example to customize:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>
    <key>Label</key>
    <string>backup-to-storagebox</string>
    <key>PATH</key>
    <string>/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin</string>
    <key>ProgramArguments</key>
    <array>
      <string>/Users/USER/.local/bin/backup-to-storagebox</string>
      <string>/Users/USER/.config/backup-to-storagebox/backup-work.json</string>
    </array>
    <key>WorkingDirectory</key>
    <string>/Users/USER</string>
    <key>StandardOutPath</key>
    <string>/tmp/backup-to-storagebox.log</string>
    <key>StandardErrorPath</key>
    <string>/tmp/backup-to-storagebox.log</string>
    <key>StartInterval</key>
    <integer>3600</integer>
    <key>RunAtLoad</key>
    <true/>
  </dict>
</plist>
```

Note that in order to use the program in a LaunchAgent, you will
need to give the `backup-to-storagebox` program full disk access.
(Settings -> Privacy and Security -> Full Disk Access.)

For testing with launchctl (macOS):
```bash
# First load it:
launchctl bootstrap gui/$(id -u) /path/to/the.plist
# Then run it manually:
launchctl kickstart gui/$(id -u)/backup-to-storagebox
```

## License

BSD-3-Clause (see LICENSE file)

## Author

John MacFarlane (jgm@berkeley.edu)
