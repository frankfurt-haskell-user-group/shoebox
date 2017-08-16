import React from 'react';
import { render } from 'react-dom';
import App from './components/App';

const remote = window.require('electron').remote;
const cprocess = remote.require('child_process');
let cp = cprocess.spawn("./backend/shoeB.exe");
/*
cp.stdout.on('data', (data) => {
		console.log(`stdout: ${data}`);
});

cp.stderr.on('data', (data) => {
  console.log(`stderr: ${data}`);
});

cp.on('close', (code) => {
  console.log(`child process exited with code ${code}`);
});
*/

render(<App name='World' cp={cp} />, document.getElementById('root'));
