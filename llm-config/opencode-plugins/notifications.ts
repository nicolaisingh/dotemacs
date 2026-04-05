import type { Plugin } from "@opencode-ai/plugin";

export const TaskNotifier: Plugin = async ({ $ }) => {
  return {
    event: async ({ event }) => {
      const platform = process.platform;
      const title = 'OpenCode';
      let message = '';

      if (event.type === "session.idle") {
        message = 'Awaiting next task';
      } else if (event.type === "permission.asked") {
        message = 'Permission needed';
      } else if (event.type === "session.error") {
        message = 'Session error';
      }

      if (!message) return;

      if (platform === "darwin") {
        await $`osascript -e 'display notification "${message}" with title "${title}"'`;
      } else if (platform === "linux") {
        await $`notify-send "${title}" "${message}"`;
      }
    },
  };
};
