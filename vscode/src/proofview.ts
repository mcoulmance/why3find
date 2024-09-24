/* ************************************************************************ */
/*                                                                          */
/*   This file is part of the why3find.                                     */
/*                                                                          */
/*   Copyright (C) 2022-2024                                                */
/*     CEA (Commissariat à l'énergie atomique et aux énergies               */
/*          alternatives)                                                   */
/*                                                                          */
/*   you can redistribute it and/or modify it under the terms of the GNU    */
/*   Lesser General Public License as published by the Free Software        */
/*   Foundation, version 2.1.                                               */
/*                                                                          */
/*   It is distributed in the hope that it will be useful,                  */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of         */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          */
/*   GNU Lesser General Public License for more details.                    */
/*                                                                          */
/*   See the enclosed LICENSE.md for details.                               */
/*                                                                          */
/* ************************************************************************ */

import * as vscode from "vscode";
import { ExtensionContext, Uri, Webview, WebviewPanel } from "vscode";

/* -------------------------------------------------------------------------- */
/* ---  Proof Tree HTML Content                                           --- */
/* -------------------------------------------------------------------------- */

function getNonce(): string {
  let text = ""
  const possible =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  for (let i = 0; i < 32; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length))
  }
  return text
}

function htmlContent(
  context: ExtensionContext,
  webview: Webview,
): string {
  const port = "5173";
  const localhost = `localhost:${port}`;
  const production = context.extensionMode == vscode.ExtensionMode.Production;
  const csp : string[] = []; // Content Security Policy
  const scr : string[] = []; // Scripts
  csp.push("default-src 'none'");

  if (production) {
    const nonce = getNonce();
    const src = webview.asWebviewUri(Uri.joinPath(
      context.extensionUri, "view", "out", "main.js"
    ));
    csp.push(`connect-src https://*`);
    csp.push(`script-src nonce-${nonce}`);
    scr.push(`<script nonce="${nonce}" src="${src}"></script>`);
  } else {
    csp.push(`connect-src https://* ws://${localhost}`);
    csp.push(`script-src 'unsafe-inline' https://* http://${localhost}`);
    scr.push(`<script type="module" src="http://localhost:${port}/src/index.tsx"></script>`);
  }

  // const reactRefresh = /*html*/ `
  //   <script type="module">
  //     import RefreshRuntime from "http://${localhost}/@react-refresh"
  //     RefreshRuntime.injectIntoGlobalHook(window)
  //     window.$RefreshReg$ = () => {}
  //     window.$RefreshSig$ = () => (type) => type
  //     window.__vite_plugin_react_preamble_installed__ = true
  //   </script>`;
  //const reactRefreshHash = "sha256-YmMpkm5ow6h+lfI3ZRp0uys+EUCt6FOyLkJERkfVnTY=";

  return /*html*/ `<!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta http-equiv="Content-Security-Policy" content="${csp.join("; ")}">
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>VSCode React Starter</title>
    </head>
    <body>
      <div id="root"></div>
      ${scr.join('\n')}
    </body>
  </html>`
}

/* -------------------------------------------------------------------------- */
/* ---  Proof Tree WebView Panel                                          --- */
/* -------------------------------------------------------------------------- */

const viewType = 'why3findProofTree';
let context: ExtensionContext | undefined;
let panel: WebviewPanel | undefined;

export function activate(ctxt: ExtensionContext): void { context = ctxt; }

function create(): void {
  if (!context) return;
  panel = vscode.window.createWebviewPanel(
    viewType, "Why3 Proof Tree", vscode.ViewColumn.Two,
    {
      enableScripts: true,
      localResourceRoots: [context.extensionUri],
    }
  );
  panel.webview.html = htmlContent(context, panel.webview);
  panel.onDidDispose(() => {
    panel = undefined;
  }, null, context.subscriptions);
}

export function deactivate(): void {
  panel?.dispose();
  context = undefined;
  panel = undefined;
}

/* -------------------------------------------------------------------------- */
/* ---  Commands                                                          --- */
/* -------------------------------------------------------------------------- */

export function show(): void {
  if (panel)
    panel.reveal();
  else
    create();
}

/* -------------------------------------------------------------------------- */
