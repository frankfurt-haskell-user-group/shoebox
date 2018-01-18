import React from 'react';
import { render } from 'react-dom';
import App from './components/App';
const remote = window.require('electron').remote;
const cprocess = remote.require('child_process');
import { Response, FileResponse, QueryResponse } from './response';

function ab2str(buf) {
    return String.fromCharCode.apply(null, new Uint8Array(buf));
}

function str2ab(str) {
  var buf = new Uint8Array(str.length);
  for (var i=0, strLen=str.length; i<strLen; i++) {
    buf[i] = str.charCodeAt(i);
  }
  return buf.buffer;
}

class ShoeboxChild {

	constructor() {
		this._log = [];
		var p = remote.process.env.SHOEBOX_PATH;
		if (!p) {p = "."}	
		if (remote.process.platform == 'win32') {
			this.cp = cprocess.spawn(p + "/backend/shoeB.exe");
		} else {
			this.cp = cprocess.spawn(p + "/backend/shoeB");
		}
		this.cp.on('close', (code) => {
		  console.log(`shoebox child process exited with code ${code}`);
		});
		this.cp.stdout.on('data', (d) => {
			var s = d.toString();
			var ms = s.split('\n');
			ms.pop();
		        var js = ms.map( m => (function () {
			    var cbor = atob(m);
		            return (new Response()).fromCBOR(str2ab(cbor));
		        } (m)) );
			js.map( (j) => {console.log("sd: ", j), this._log.push(j); $(this).trigger('shoebox-data', j ); });
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

	callShoeboxCmd (cmd) {
	    var cbor = cmd.toCBOR(); 
	    var cbor64 = btoa(ab2str(cbor));
            this.cp.stdin.write( cbor64 + "\n");
	}

}

const sbc = new ShoeboxChild()

render(<App name='World' sbc={sbc}/>, document.getElementById('root'));
