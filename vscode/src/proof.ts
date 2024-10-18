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
import { ExtensionContext, Webview, WebviewPanel, Disposable } from "vscode";
import * as Client from './client';

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
  const csp: string[] = []; // Content Security Policy
  const scr: string[] = []; // Scripts
  csp.push("default-src 'none'");

  const nonce = getNonce();
  const src = webview.asWebviewUri(vscode.Uri.joinPath(
    context.extensionUri, "proofview", "out", "main.js"
  ));
  csp.push(`connect-src https://*`);
  csp.push(`script-src 'nonce-${nonce}'`);
  scr.push(`<script type="module" nonce="${nonce}" src="${src}"></script>`);

  return /*html*/ `<!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta http-equiv="Content-Security-Policy" content="${csp.join("; ")}" />
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

let panel: WebviewPanel | undefined;
let context: ExtensionContext | undefined;

function create(): void {

  // Context
  if (!context) return;
  panel?.dispose();

  // Create Panel
  panel = vscode.window.createWebviewPanel(
    'why3findProofTree',
    'Why3 Proof Tree',
    vscode.ViewColumn.Two,
    {
      enableScripts: true,
      retainContextWhenHidden: true,
      localResourceRoots: [context.extensionUri],
    }
  );
  panel.webview.html = htmlContent(context, panel.webview);

  // Client Notifications
  // Client.onStatus((data: Client.Status) => {
  //   panel?.webview.postMessage({ command: 'status', data });
  // }, undefined, context.subscriptions);

  // Panel Callbacks
  // panel.webview.onDidReceiveMessage(msg => {
  //   switch (msg.command) {
  //     case 'interrupt':
  //       Client.interrupt();
  //       break;
  //   }
  // }, undefined, context.subscriptions);

  // Panel Disposed
  panel.onDidDispose(() => {
    panel = undefined;
  }, undefined, context.subscriptions);
}

/* -------------------------------------------------------------------------- */
/* ---  Commands                                                          --- */
/* -------------------------------------------------------------------------- */

export function show(): void {
  if (panel) panel.reveal();
  else create();
}

export function activate(ctxt: ExtensionContext): void {
  context = ctxt;
}

export function deactivate(): void {
  panel?.dispose();
  panel = undefined;
  context = undefined;
}

/* -------------------------------------------------------------------------- */
