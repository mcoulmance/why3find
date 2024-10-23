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
import '@vscode/codicons/dist/codicon.css';
import '@vscode/codicons/dist/codicon.ttf';

type ClassName = undefined | false | string ;

function classes(...names : ClassName[]): string {
    return names.filter(a => !!a).join(' ');
}

/* -------------------------------------------------------------------------- */
/* ---  Icon                                                              --- */
/* -------------------------------------------------------------------------- */

export interface IconProps {
    name?: string;
}

export function Icon(props: IconProps): JSX.Element | null {
    const { name } = props;
    if (!!name) {
        const className = `vsicon codicon codicon-${name}`;
        return <span className={className} />;
    }
    return null;
}

/* -------------------------------------------------------------------------- */
/* ---  Label                                                             --- */
/* -------------------------------------------------------------------------- */

export interface LabelProps {
    icon?: string;
    label?: string;
    title?: string;
    children?: React.ReactNode;
}

export function Label(props: LabelProps): JSX.Element {
    return (
        <div className='vstext vslabel' title={props.title} >
            <Icon name={props.icon} />
            <label>{props.label}{props.children}</label>
        </div>
    );
}

/* -------------------------------------------------------------------------- */
/* ---  Button                                                            --- */
/* -------------------------------------------------------------------------- */

export interface ButtonProps {
    icon?: string;
    label?: string;
    title?: string;
    display?: boolean;
    enabled?: boolean;
    onClick?: () => void;
}

export function Button(props: ButtonProps): JSX.Element {
    const { display=true, enabled=true } = props;
    const className = classes('vstext', 'vsbutton', !display && 'vsnone' );
    return (
        <button
            className={className}
            title={props.title}
            disabled={!enabled}
            onClick={props.onClick}
        ><Icon name={props.icon} />{props.label}</button>
    );
}

/* -------------------------------------------------------------------------- */
