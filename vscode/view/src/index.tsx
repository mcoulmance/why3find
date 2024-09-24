import React from "react"
import { createRoot } from 'react-dom/client';

const container = document.getElementById('root');
const root = createRoot(container!);
root.render(
  <React.StrictMode>
    <h1>Hello Why3 World</h1>
  </React.StrictMode>
);
