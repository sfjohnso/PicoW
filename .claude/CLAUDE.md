# CLAUDE.md — working agreement for Steve's machine

This file holds the **always-true** rules for how to work here. Project-specific
details (board ports, per-project state) live in the memory store, not here.

## How to work

1. **Think before coding.** State assumptions out loud. When a request has two
   readings, say so and ask — don't silently pick one. When a simpler approach
   exists, push back before building the complex one.

2. **Simplicity first.** Write the minimum that solves the problem. No speculative
   features, no abstractions for code used once, no error handling for things that
   cannot happen. If a senior engineer would call it overcomplicated, cut it.

3. **Surgical changes.** Touch only what the task requires. Don't "improve" the
   code next door, don't reformat things that already work, and match the existing
   style even when you'd do it differently.

4. **Goal-driven execution.** Turn every task into something you can verify. Where
   a test harness fits: write tests for bad inputs, then make them pass; write a
   test that reproduces a bug, then make it pass. Where it doesn't (embedded /
   scripts), verify by actually running it — on the board, on the file — and
   report the observed result, not a promise.

5. **Investigate before fixing.** No fix without a root cause. Never claim a test
   was "pre-existing," that something "works," or that a failure is "unrelated"
   without proof (e.g., run it on the base state to show it). "Unverified" is an
   honest thing to say; unsupported attribution is not.

6. **Don't abandon long-running work.** Poll to completion rather than handing back
   a "I'll check later." If something genuinely can't finish, say what's blocking.

7. **Ask, don't guess — then persist.** Use a question only when the answer is
   genuinely the user's call (scope, a real trade-off). Once answered, record it in
   memory so the same question isn't asked twice.

8. **Communication.** Lead with the outcome, not the mechanics. Short and direct;
   concrete numbers over adjectives ("~0.7s/request," not "fast"). No marketing
   words ("robust," "seamless," "delve").

## Error-handling philosophy (refines rule 2)

- Guard against errors that can actually occur; skip guards for impossible states.
- Use **typed** catches when you know the expected failure; only swallow errors
  wholesale for genuine best-effort cleanup, never to hide real failures.

## Environment (Windows 11)

- **PowerShell 5.1** is primary; **Git Bash** is available — use each one's syntax.
- Quoting traps (learned the hard way):
  - Single-quoted here-strings **double apostrophes** in written files.
  - PS 5.1 **strips embedded double-quotes** passed to native exes (e.g. `python`)
    — drive serial boards via command-files or quote inside Python instead.
- **Elevation:** the PowerShell session is **not** elevated. For admin tasks, write
  a `.ps1`, launch it with `Start-Process -Verb RunAs`, log to `%TEMP%`, and read
  the log back. Keep reusable scripts in `C:\Users\Steve\tools\`.

## Git safety (this matters)

- The home directory `C:\Users\Steve` **is the repo root and contains secrets.**
  **Never bulk-add** (`git add -A`/`git add .`). The whitelist `.gitignore` is the
  backstop; track a new file with an explicit `!path` entry.
- Commit and push **only when asked.** One logical change per commit. The commit
  message describes **what shipped**, not the path you took to get there. End commit
  messages with the `Co-Authored-By: Claude ...` trailer.

## Don't touch / confirm first

- Never disable **Windows Update, Defender, or the firewall.**
- No **BitLocker** changes without capturing the recovery key first.
- **Warn and confirm** before destructive or hard-to-reverse actions: `rm -rf`,
  registry deletes, force-push, uninstalling software, removing services.

## Hardware / serial workflow

- **MicroPython / ESP32-S3:** `mpremote` on the running REPL port; `esptool` on the
  bootloader port. Mind the dual-port quirk (bootloader vs REPL re-enumerate as
  different COM ports); press **RESET, not BOOT,** to leave download mode.
- **WebMite / MMBasic Picos:** XMODEM + the `serial_cmd.py` helper. Per-board COM
  mappings and board quirks live in the memory files.

## Output conventions

- Finished deliverables (letters, spreadsheets, HTML) go to the **Desktop** unless
  told otherwise.
