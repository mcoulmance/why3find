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

/* -------------------------------------------------------------------------- */
/* --- Why3 LSP Client                                                    --- */
/* -------------------------------------------------------------------------- */

import {
    ExtensionContext,
    StatusBarItem,
    StatusBarAlignment,
    ThemeColor,
    window,
} from "vscode";

import * as Client from './client';

/* -------------------------------------------------------------------------- */
/* --- Updating Status Bar Item                                           --- */
/* -------------------------------------------------------------------------- */

let status : StatusBarItem | undefined;

const StatusFailures = new ThemeColor('statusBarItem.warningBackground');

function updateStatus(s: Client.Status): void {
	if (!status) return;
	const { pending = 0, running = 0, failed = 0} = s;
	status.text = `$(debug-alt-small) ${pending} / ${running}` ;
	status.command = 'why3.proof.show' ;
	status.tooltip = [
		running && `Why3 Running provers : ${running}`,
		`Why3 Pending tasks : ${pending}`,
		failed && `Why3 Failed tasks : ${failed}`,
	].filter(s => s).join('\n');
	status.backgroundColor = failed ? StatusFailures : undefined ;
}

/* -------------------------------------------------------------------------- */
/* --- Extension Registration                                             --- */
/* -------------------------------------------------------------------------- */

export function activate(ctxt : ExtensionContext) : void
{
	if (status) status.dispose();
	status = window.createStatusBarItem(StatusBarAlignment.Left);
	ctxt.subscriptions.push(
        status,
        Client.onStatus(updateStatus)
    );
	status.show();
	updateStatus({});
}

export function deactivate() {
	if (status) status.dispose();
	status = undefined;
};

/* -------------------------------------------------------------------------- */
