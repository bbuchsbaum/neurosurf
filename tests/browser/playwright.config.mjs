const { defineConfig } = await import(new URL(
  "../../../jscode/surfviewjs/node_modules/@playwright/test/index.mjs",
  import.meta.url
));

export default defineConfig({
  testDir: new URL(".", import.meta.url).pathname,
  testMatch: "rfe77.spec.mjs",
  timeout: 30000,
  workers: 1,
  reporter: "line",
  use: {
    browserName: "chromium",
    headless: true,
    acceptDownloads: true
  }
});
