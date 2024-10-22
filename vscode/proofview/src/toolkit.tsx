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

/** VSCode WebView Toolkit */

import * as React from 'react';
import './style.css';

type ClassName = undefined | false | string ;

function classes(...names : ClassName[]): string {
    return names.filter(a => !!a).join(' ');
}

/* -------------------------------------------------------------------------- */
/* ---  Label                                                             --- */
/* -------------------------------------------------------------------------- */

export interface LabelProps {
    label?: string;
    title?: string;
    children?: React.ReactNode;
}

export function Label(props: LabelProps): JSX.Element {
    return (
        <label
            className='vstext vslabel'
            title={props.title}
        >{props.label}{props.children}</label>
    );
}

/* -------------------------------------------------------------------------- */
/* ---  Button                                                            --- */
/* -------------------------------------------------------------------------- */

export interface ButtonProps {
    label?: string;
    title?: string;
    display?: boolean;
    enabled?: boolean;
    onClick?: () => void;
}

export function Button(props: ButtonProps): JSX.Element {
    const { display=true, enabled=true } = props;
    const className = classes(
        'vstext', 'vsbutton',
        !display && 'vsnone',
    );
    return (
        <button
            className={className}
            title={props.title}
            disabled={!enabled}
            onClick={props.onClick}
        >{props.label}</button>
    );
}

/* -------------------------------------------------------------------------- */
