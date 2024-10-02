import { resolve } from "path"
import { defineConfig } from "vite"
import react from "@vitejs/plugin-react"

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  build: {
    outDir: "out",
    lib: {
      entry: resolve(__dirname,"src/index.tsx"),
      fileName: "main",
      formats: [ "es" ]
    }
  },
  define: {
    'process.env': {}
  },
  server: {
    hmr: {
      host: "localhost",
      protocol: "ws",
    },
  },
})
