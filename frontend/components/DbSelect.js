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

    openDatabase() {
    	var dbName = this.fileWhichWillBeOpened;
		this.props.sbc.callShoebox("open-db", dbName);
		this.setState({dbFile : dbName});
		$('#modalIdOpenDB').modal('hide');	
    }

    deleteDatabase() {
    	// check, if we have still one to open, after we deleted the current one
    	if (this.state.availableDBs.length >= 2) {
    		// delete will open a new db
			this.props.sbc.callShoebox("delete-db", this.state.dbFile); 
		} else {
			this.props.statusF("cannot delete last DB");
		}
		$('#modalIdDeleteDB').modal('hide');	
    }

    newDatabase() {
    	var dbName = $('#newDbName')[0].value;
		this.props.sbc.callShoebox("new-db", dbName); 
		$('#modalIdNewDB').modal('hide');	
    }

    saveAsDatabase() {
    	var dbName = $('#newSaveAsDbName')[0].value;
		this.props.sbc.callShoebox("save-db-as", dbName); 
		$('#modalIdSaveAsDB').modal('hide');	
    }

    saveDatabase() {
		this.props.sbc.callShoebox("save-db", null);
    }


	getBackendState() {
		this.props.sbc.callShoebox("available-dbs", null);
	}

	update(e, d) {
		if (d.msg == "res") {
			this.props.statusF(d.para);
		}
		if (d.msg == "dbs") {
			this.setState({availableDBs : d.para.availableDBs, dbFile : d.para.dbFile }); 
		}
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
					&nbsp;<button className="btn btn-default btn-sm" data-toggle="modal" data-target="#modalIdSaveAsDB">Save As <span className="glyphicon glyphicon-save"></span></button>
					&nbsp;<button className="btn btn-default btn-sm" data-toggle="modal" data-target="#modalIdNewDB">New <span className="glyphicon glyphicon-plus-sign"></span></button>
					&nbsp;<button className="btn btn-danger btn-sm" data-toggle="modal" data-target="#modalIdDeleteDB">Delete <span className="glyphicon glyphicon-remove-sign"></span></button>
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
			{modalDialog(
				"modalIdSaveAsDB"	
				, "Save Database As ..."
				, <div className="form-group">
					<label for="newDbName">Database Name:</label>
					<input type="text" className="form-control" id="newSaveAsDbName"></input>
				  </div>
				, <div>
					<button className="btn btn-default" data-dismiss="modal">Cancel</button>
					&nbsp;<button className="btn btn-primary" onClick={ this.saveAsDatabase }>Save</button>
				  </div> 
			)}
			{modalDialog(
				"modalIdNewDB"	
				, "New Database"
				, <div className="form-group">
					<label for="newDbName">Database Name:</label>
					<input type="text" className="form-control" id="newDbName"></input>
				  </div>
				, <div>
					<button className="btn btn-default" data-dismiss="modal">Cancel</button>
					&nbsp;<button className="btn btn-primary" onClick={ this.newDatabase }>Create</button>
					</div> 
			)}
			{modalDialog(
				"modalIdDeleteDB"	
				, "Delete Database"
				, <div>Are you sure, you want to delete database <b>{this.state.dbFile}</b> ?</div>
				, <div>
					<button className="btn btn-default" data-dismiss="modal">Cancel</button>
					&nbsp;<button className="btn btn-danger" onClick={ this.deleteDatabase }>Yes, go ahead!</button>
					</div> 
			)}
			</div>
  		);
  	}

  // constructor
   constructor(props) {
    super(props);
    this.state = { dbFile : "", availableDBs : [] };
    this.openDatabase = this.openDatabase.bind(this);
    this.saveDatabase = this.saveDatabase.bind(this);
    this.saveAsDatabase = this.saveAsDatabase.bind(this);
    this.newDatabase = this.newDatabase.bind(this);
    this.deleteDatabase = this.deleteDatabase.bind(this);
    this.getBackendState = this.getBackendState.bind(this);
    this.update = this.update.bind(this);
    this.getBackendState();
	}

    componentDidMount() {
      this.props.sbc.subscribe(this.update);
    }

    componentWillUnmount() {
      this.props.sbc.unsubscribe(this.update);
    }
}

export default DbSelect; 