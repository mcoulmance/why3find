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

import {
  window,
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

  constructor() { super(SOCKADDR, 'Why3 DAP'); }

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
        config.debug = true;
      }
    }
    return config;
  }

  // Adapter Factory
  createDebugAdapterDescriptor(
    session: DebugSession,
    executable: DebugAdapterExecutable | undefined
  ): ProviderResult<DebugAdapterDescriptor> {
    if (session.type !== 'why3') return null;
    if (executable !== undefined) return null;
    const { file, debug } = session.configuration;
    this.log(`${debug ? 'debug' : 'prove'} ${file}`);
    return this
      .spawn("why3find", "dap", "--pipe", SOCKADDR, debug ? '--debug' : '--prove')
      .then(() => new DebugAdapterNamedPipeServer(SOCKADDR));
  }

}
