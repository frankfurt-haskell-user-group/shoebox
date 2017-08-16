import React, { Component } from 'react';
import ListGroup from './ListGroup';

function modalDialog(myId, myTitle, myContent, myFooter) {
	return (
		<div id={myId} className="modal fade" role="dialog">
			<div className="modal-dialog">
				<div className="modal-content">
					<div className="modal-header">
						<button className="close" data-dismiss="modal">×</button>
						<h4 className="modal-title">{myTitle}</h4>
						</div>
					<div className="modal-body">
						{myContent}
						</div>
					<div className="modal-footer">
						{myFooter}
						</div>
					</div>
				</div>
			</div>
		);
}

class DbSelect extends React.Component {

	backendCmd(cmd, f) {
		this.props.cp.stdout.once("data", function (d) {
			let r = JSON.parse(d.toString());
			f(r);
		});
		this.props.cp.stdin.write("{\"cmd\" : \"" + cmd + "\"}\n");
	}

    openDatabase() {
    	this.props.statusF("file opened: " + this.fileWhichWillBeOpened);
		$('#modalIdOpenDB').modal('hide');	
    }

    saveDatabase() {
		this.backendCmd("save-db", 
			(d) => { this.props.statusF(d); }
		);
    }


	getAvailableDBs() {
		this.backendCmd("available-dbs", 
			(d) => { this.setState({availableDBs : d}); }
		);
	}

  // render function 
  render() {
  	return (
  		<div>
	  		<div className="row top-row">
				<div className="col-sm-6">
					<h3><small>selected database: </small>{this.state.dbFile}</h3>
					</div>
				<div className="col-sm-6 file-buttons">
					<button className="btn btn-primary btn-sm" data-toggle="modal" data-target="#modalIdOpenDB">Open <span className="glyphicon glyphicon-open"></span></button>
					&nbsp;<button className="btn btn-default btn-sm" onClick={this.saveDatabase}>Save <span className="glyphicon glyphicon-save"></span></button>
					&nbsp;<button className="btn btn-default btn-sm">Save As <span className="glyphicon glyphicon-save"></span></button>
					&nbsp;<button className="btn btn-default btn-sm">New <span className="glyphicon glyphicon-plus-sign"></span></button>
					&nbsp;<button className="btn btn-danger btn-sm">Delete <span className="glyphicon glyphicon-remove-sign"></span></button>
					</div>
  			</div>
			{modalDialog(
				"modalIdOpenDB"	
				, "Open Database"
				, <ListGroup items={ this.state.availableDBs } onchange={ (t) => this.fileWhichWillBeOpened = t } />
				, <div>
					<button className="btn btn-default" data-dismiss="modal">Cancel</button>
					&nbsp;<button className="btn btn-primary" onClick={ this.openDatabase }>Open</button>
					</div> 
				)}
			</div>
  		);
  	}

  // constructor
   constructor(props) {
    super(props);
    this.state = { dbFile : "frz", availableDBs : [] };
    this.openDatabase = this.openDatabase.bind(this);
    this.saveDatabase = this.saveDatabase.bind(this);
    this.getAvailableDBs();
	}

}

export default DbSelect; 