/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Why3 LSP Client                                                    --- */
/* -------------------------------------------------------------------------- */

import { access } from 'fs/promises';
import { setTimeout } from 'timers/promises';

import {
  window,
  debug,
  Disposable,
  DebugConfiguration,
  DebugConfigurationProvider,
  ProviderResult,
  WorkspaceFolder,
  DebugAdapterDescriptorFactory,
  DebugAdapterDescriptor,
  DebugAdapterExecutable,
  DebugSession,
  DebugAdapterNamedPipeServer,
} from 'vscode';

import Server from './rpc';

/* -------------------------------------------------------------------------- */
/* --- Debug Adapater Proxy                                               --- */
/* -------------------------------------------------------------------------- */

const SOCKADDR = '/tmp/why3find.dap.io';

export default class Adapter extends Server implements
  DebugConfigurationProvider,
  DebugAdapterDescriptorFactory,
  Disposable {

  constructor() {
    super(SOCKADDR, 'Why3 DAP');
    this.startDebugging = this.startDebugging.bind(this);
  }

  // Debug Configuration
  resolveDebugConfiguration(
    folder: WorkspaceFolder | undefined,
    config: DebugConfiguration,
  ): ProviderResult<DebugConfiguration> {
    if (!config.type && !config.name && !config.request) {
      const editor = window.activeTextEditor;
      if (editor && editor.document.languageId === 'why3') {
        config.type = 'why3';
        config.name = 'Debug';
        config.request = 'launch';
        config.file = '${file}';
      }
    }
    return config;
  }

  startDebugging() {
    const editor = window.activeTextEditor;
    if (editor && editor.document.languageId === 'why3') {
      const config: DebugConfiguration = {
        type: 'why3',
        name: 'Debug',
        request: 'launch',
        file: editor.document.uri.fsPath,
      }
      debug.startDebugging(undefined, config);
    }
  }

  private async launchAdapter(file: string): Promise<DebugAdapterDescriptor> {
    // Spawn Server
    await this.spawn("why3find", "dap", "--pipe", SOCKADDR, file);
    // Wait for the socket to be accessible
    let starting = true;
    while (starting) {
      try {
        await access(SOCKADDR);
        starting = false;
      } catch {
        this.log('waiting...');
        await setTimeout(100);
      }
    }
    // Connect to the Adapter
    this.log('connecting');
    return new DebugAdapterNamedPipeServer(SOCKADDR);
  }

  // Adapter Factory
  createDebugAdapterDescriptor(
    session: DebugSession,
    executable: DebugAdapterExecutable | undefined
  ): ProviderResult<DebugAdapterDescriptor> {
    if (session.type !== 'why3') return null;
    if (executable !== undefined) return null;
    const file = session.configuration.file.toString();
    return this.launchAdapter(file);
  }

}
