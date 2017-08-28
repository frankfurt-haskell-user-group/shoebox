import React from 'react';
import { render } from 'react-dom';
import App from './components/App';
const remote = window.require('electron').remote;
const cprocess = remote.require('child_process');

class ShoeboxChild {

	constructor() {
		this._log = [];
		if (remote.process.platform == 'win32') {
			this.cp = cprocess.spawn("./backend/shoeB.exe");
		} else {
			this.cp = cprocess.spawn("./backend/shoeB");
		}
		this.cp.on('close', (code) => {
		  console.log(`shoebox child process exited with code ${code}`);
		});
		this.cp.stdout.on('data', (d) => {
			var s = d.toString();
			console.log("bin data: " + s);
			var ms = s.split('\n');
			ms.pop();
			var js = ms.map( (m) => JSON.parse(m));
			js.map( (j) => {console.log(j), this._log.push(j); $(this).trigger('shoebox-data', j ); });
		});
	}

	getLog() {
		return this._log;
	}

	subscribe(f) {
		$(this).on('shoebox-data', f);
	}

	unsubscribe(f) {
		$(this).off('shoebox-data', f);
	}

	callShoebox(cmd, para) {
/*		this.cp.stdout.once("data", function (d) {
			let r = d.toString();
			f(r);
		});
*/
		var msg;
		if (para == null) {
			msg = JSON.stringify({cmd : cmd});
		} else {
			msg = JSON.stringify({cmd : cmd, para : para});
		}
		this.cp.stdin.write( msg + "\n");
		this._log.push(JSON.parse(msg));
	}
}

const sbc = new ShoeboxChild()

render(<App name='World' sbc={sbc}/>, document.getElementById('root'));
