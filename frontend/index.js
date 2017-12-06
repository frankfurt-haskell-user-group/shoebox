import './css/base.css';
import React from 'react';
// import { render } from 'react-dom';
import { app } from './components/App';
const remote = window.require('electron').remote;
// const cprocess = remote.require('child_process');
// import { Response, FileResponse, QueryResponse } from './response';

// render(<App name='World' sbc={sbc}/>, document.getElementById('root'));
app();
