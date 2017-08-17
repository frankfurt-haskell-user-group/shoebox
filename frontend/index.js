import React from 'react';
import { render } from 'react-dom';
import App from './components/App';
const remote = window.require('electron').remote;
const cprocess = remote.require('child_process');

class ShoeboxChild {

	constructor() {
		this.cp = cprocess.spawn("./backend/shoeB.exe");
		this.cp.on('close', (code) => {
		  console.log(`shoebox child process exited with code ${code}`);
		});
	}

	callShoebox(cmd, para, f) {
		this.cp.stdout.once("data", function (d) {
			let r = d.toString();
			f(r);
		});
		if (para == null) {
			this.cp.stdin.write( JSON.stringify({cmd : cmd}) + "\n");
		} else {
			this.cp.stdin.write( JSON.stringify({cmd : cmd, para : para}) + "\n");
		}
	}
}

const sbc = new ShoeboxChild()

render(<App name='World' sbc={sbc}/>, document.getElementById('root'));
