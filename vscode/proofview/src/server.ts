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

import React from 'react';
import { EventEmitter } from 'events';
import { WebviewApi } from 'vscode-webview';

const vscode : WebviewApi<any> = acquireVsCodeApi();
const emitter = new EventEmitter();
{
    window.addEventListener('message', evt => {
        const { command, data } = evt.data;
        emitter.emit(command, data);
    });
}

export interface Callback<A> { (data: A): void }

export function useCommand<A>(cmd: string, fn?: Callback<A>): void
{
    const ref = React.useRef<Callback<A>>();
    ref.current = fn;
    React.useEffect(() => {
        const callback = (data: A) => {
            const fn = ref.current;
            if (fn) fn(data);
        };
        emitter.addListener(cmd, callback);
        return () => { emitter.removeListener(cmd, callback); };
    }, [ cmd ]);
}

export function useValue<A>(cmd: string): A | undefined
{
    const [value,setValue] = React.useState<A>();
    React.useEffect(() => {
        emitter.addListener(cmd, setValue);
        return () => { emitter.removeListener(cmd, setValue); };
    }, [ cmd ]);
    return value;
}

export function sendCommand<A>(command: string, data: A): void {
    vscode.postMessage({ command, data });
}
