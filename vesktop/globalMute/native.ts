// Runs in Vesktop's main process. `vesktop-mute` touches a file in the trigger
// dir; we consume it and tell the renderer to toggle mute.

import { IpcMainInvokeEvent } from "electron";
import { FSWatcher, mkdirSync, readdirSync, rmSync, watch } from "fs";
import { join } from "path";

let watcher: FSWatcher | undefined;

// Only use the per-user runtime dir; a shared dir like /tmp would let other
// users trigger the toggle
function getTriggerDir() {
    const runtimeDir = process.env.XDG_RUNTIME_DIR;
    if (!runtimeDir) throw new Error("GlobalMute: XDG_RUNTIME_DIR is not set");
    return join(runtimeDir, "vesktop-global-mute");
}

function consumeTriggers(dir: string) {
    const names = readdirSync(dir);
    for (const name of names) rmSync(join(dir, name), { force: true });
    return names;
}

export function startWatching(e: IpcMainInvokeEvent) {
    watcher?.close();
    const dir = getTriggerDir();
    mkdirSync(dir, { recursive: true, mode: 0o700 });
    // Drop triggers left over from before Vesktop started
    consumeTriggers(dir);

    watcher = watch(dir, () => {
        if (!consumeTriggers(dir).includes("mute")) return;
        e.sender.executeJavaScript("Vencord.Plugins.plugins.GlobalMute.toggleMute()").catch(() => {});
    });
}

export function stopWatching() {
    watcher?.close();
    watcher = undefined;
}
