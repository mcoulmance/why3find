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

import React from "react"
import { createRoot } from 'react-dom/client';

function Counter() : JSX.Element {
  const [ n, setN ] = React.useState(0);
  const onAdd = () => setN(n+1);
  const onReset = () => setN(0);
  return (
    <div>
      <button>Proofs {n}</button>
      <button onClick={onAdd}>ADD</button>
      <button onClick={onReset}>RESET</button>
    </div>
  );
}

const container = document.getElementById('root');
const root = createRoot(container!);
root.render(
  <React.StrictMode>
    <h1>Hello Why3 Amazing World</h1>
    <Counter />
  </React.StrictMode>
);
