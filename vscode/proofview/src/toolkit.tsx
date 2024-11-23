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

type ClassName = undefined | false | string;

function classes(...names: ClassName[]): string {
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
        <div className='vslabel' title={props.title} >
            <Icon name={props.icon} />
            <label className='vstext'>{props.label}{props.children}</label>
        </div>
    );
}

/* -------------------------------------------------------------------------- */
/* ---  Button                                                            --- */
/* -------------------------------------------------------------------------- */

export interface ButtonProps {
    className?: string;
    icon?: string;
    label?: string;
    title?: string;
    display?: boolean;
    enabled?: boolean;
    secondary?: boolean;
    onClick?: () => void;
}

export function Button(props: ButtonProps): JSX.Element {
    const { display = true, enabled = true, secondary = false } = props;
    const className = classes(
        'vsbutton', props.className,
        !display && 'vsnone',
        secondary && 'secondary',
        enabled ? 'enabled' : 'disabled',
    );
    return (
        <div
            className={className}
            title={props.title}
            onClick={enabled ? props.onClick : undefined}
        >
            <Icon name={props.icon} />
            <span className="vstext">{props.label}</span>
        </div>
    );
}

/* -------------------------------------------------------------------------- */
/* ---  Spaces                                                            --- */
/* -------------------------------------------------------------------------- */

export interface SpaceProps {
    className?: string;
    width?: number;
    height?: number;
}

export function Space(props: SpaceProps): JSX.Element {
    const { width = 12, height = 12 } = props;
    const className = classes('vspack', props.className);
    return <div className={className} style={{ width, height }} />;
}

export function Filler(props: SpaceProps): JSX.Element {
    const { width = 12, height = 12 } = props;
    const className = classes('vsexpand', props.className);
    return <div className={className} style={{ width, height }} />;
}

/* -------------------------------------------------------------------------- */
/* ---  Boxes                                                             --- */
/* -------------------------------------------------------------------------- */

export interface BoxProps {
    pack?: boolean;
    fill?: boolean;
    display?: boolean;
    className?: string;
    children: React.ReactNode;
}

interface ContainerProps extends BoxProps {
    baseClass: string;
}

function Container(props: ContainerProps): JSX.Element {
    const { display = true, pack = false, fill = false } = props;
    const className = classes(
        props.baseClass,
        props.className,
        pack && 'vspack',
        fill && 'vsfill',
        !display && 'vsnone',
    );
    return <div className={className}>{props.children}</div>;
}

export function Hbox(props: BoxProps): JSX.Element {
    return <Container baseClass='vshbox' {...props} />;
}

export function Vbox(props: BoxProps): JSX.Element {
    return <Container baseClass='vshbox' {...props} />;
}

/* -------------------------------------------------------------------------- */
