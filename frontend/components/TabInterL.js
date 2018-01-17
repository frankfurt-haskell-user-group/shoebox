import React, { Component } from 'react';
import { Command, FileCommand, QueryCommand } from '../commands';
import { Response, FileResponse, QueryResponse } from '../response';

/**
 * Creates a string that can be used for dynamic id attributes
 * Example: "id-so7567s1pcpojemi"
 * @returns {string}
 */
var uniqueId = function() {
    return 'id-' + Math.random().toString(36).substr(2, 16);
}

var colsTable = function (level, elId, sbc, cols, result) {
    var l = cols.length;
    if (typeof(result) == "string") {
        return (
                <div>{result}</div>
        );
    }
    if (typeof(result) == "object") {
    return (
            <table className={(level == 0) ? "table table-sm table-bordered" : "table table-sm"} style={{marginBottom:'5px', padding:'0px'}}>
              <tr>
                <td style={{width:100/(l+1-level)+'%'}}>
                  {Object.keys(result)[0]}
                </td>
                <td>
                  <table className="table table-sm" style={{marginBottom:'0px', padding:'0px'}}>
                  {Object.values(result)[0].length ? Object.values(result)[0].map(
                      (c) => {
                          return (
                                    <tr>
                                      <td>
                                  <table className={(level == 0) ? "table table-sm table-bordered" : "table table-sm"} style={{marginBottom:'0px', padding:'0px'}}>
                                  {c.map(
                                      (r) => {
                                          if (typeof(r) == "string" || typeof(r) == "object") {
                                          return (
                                                  <tr><td>
                                                  {colsTable(level+1, elId, sbc, cols, r)}
                                                  </td></tr>
                                            );
                                            }
                                        }
                                    )}
                                    </table>
                                        </td>
                                        </tr>
                            );
                        }
                  ) : (<ChangeL sbc={sbc} word={Object.keys(result)[0]} level={level} cols={cols} superId={elId}>
                        </ChangeL>)}
                  </table>
                </td>
              </tr>
            </table>
    );
    }
}

Object.compare = function (obj1, obj2) {
	  //Loop through properties in object 1
	  for (var p in obj1) {
		    //Check property exists on both objects
		    if (obj1.hasOwnProperty(p) !== obj2.hasOwnProperty(p)) return false;
        
		    switch (typeof (obj1[p])) {
			      //Deep compare objects
			  case 'object':
				    if (!Object.compare(obj1[p], obj2[p])) return false;
				    break;
			      //Compare function code
			  case 'function':
				    if (typeof (obj2[p]) == 'undefined' || (p != 'compare' && obj1[p].toString() != obj2[p].toString())) return false;
				    break;
			      //Compare values
			  default:
				    if (obj1[p] != obj2[p]) return false;
		    }
	  }
    
	  //Check object 2 for any extra properties
	  for (var p in obj2) {
		    if (typeof (obj1[p]) == 'undefined') return false;
	  }
	  return true;
};

class ChangeL extends React.Component {

    // render function
    render () {
        var lookups = this.props.cols[this.props.level].lookups;
        var myId = this.props.superId;
        return (
            <div>
                <div className="form-group">
                <select className="form-control" id={myId + "db-chooser"}>
                {lookups.map( (l) => {
                    return (
                            <option>{(l.tag == "LUKey") ? l.contents.sbdiName : l.contents[0].sbdiName}</option>
                    );
                })}
                </select>
                <input type="text" className="form-control" id={myId + "word-translation"}></input>
                <button className="btn btn-default btn-sm" type="button" onClick={ () => {
                    var trans = $('#' + myId + 'word-translation')[0].value;
                    var db = $('#' + myId + 'db-chooser')[0].value;
                    this.sendNewData(this.props.word, trans, db, lookups, this.props.level, this.props.cols);
            }}>Submit</button>
                </div>
            </div>
        );
    }

    sendNewData(word, trans, db, lookups, level, cols) {
        // check different lookup options
        lookups.map( (l) => {
            if (db == (l.tag == "LUKey" ? l.contents.sbdiName : l.contents[0].sbdiName)) {
                var memo = null;
                if (l.tag == "LUKey") {
                    // we need to get db tab from next level :-( maybe design of LU.. can be optimized
                    var levels2 = cols[level+1].lookups;
                    for (var l2 = 0; l2 < levels2.length; l2++) {
                        if (db == levels2[l2].contents[0].sbdiName) {
                            memo = levels2[l2].contents[1].sbdtMemo;
                        }
                    }
                } else {
                    memo = l.contents[1].sbdtMemo;
                }
                if (memo) {
                    console.log("dbUpdate", word, trans, db, memo, this.props.superId);
                    this.props.sbc.callShoeboxCmd(new Command(Command.CmdQuery, new QueryCommand(QueryCommand.QueryInsertWord, this.props.superId, word, trans, db, memo)));
                }
            }
        })
    }

    // constructor
    constructor(props) {
        super(props);
        this.state = {  };
        this.update = this.update.bind(this);
        this.sendNewData = this.sendNewData.bind(this);
    }

    update(e, d) {
        if (d.selector == Response.ResQuery && d.record[0].selector == QueryResponse.QueryTransWord && d.record[0].record[0] == this.state.id)
        {
        }
    }

    componentDidMount() {
        this.props.sbc.subscribe(this.update);
    }

    componentDidUpdate() {
    }

    componentWillUnmount() {
        this.props.sbc.unsubscribe(this.update);
    }
 
}


class WordL extends React.Component {

    // render function
    render () {
        return (
            <div>
                {colsTable(0, this.state.id, this.props.sbc, this.props.cols, this.state.result)}
            </div>
        );
    }

    // constructor
    constructor(props) {
        super(props);
        this.state = { id : uniqueId(), result: "" };
        this.update = this.update.bind(this);
    }

    update(e, d) {
        if (d.selector == Response.ResQuery && d.record[0].selector == QueryResponse.QueryTransWord && d.record[0].record[0] == this.state.id)
        {
            var newResult = JSON.parse(d.record[0].record[1]);
            if (!Object.compare(this.state.result, newResult)) {
                this.setState( { result : newResult } );
            }
        }
    }

    componentDidMount() {
        this.props.sbc.subscribe(this.update);
        this.props.sbc.callShoeboxCmd(new Command(Command.CmdQuery, new QueryCommand(QueryCommand.QueryTransWord, this.state.id, this.props.word)));
    }

    componentDidUpdate() {
        this.props.sbc.callShoeboxCmd(new Command(Command.CmdQuery, new QueryCommand(QueryCommand.QueryTransWord, this.state.id, this.props.word)));
    }

    componentWillUnmount() {
        this.props.sbc.unsubscribe(this.update);
    }
 
}



class TabInterL extends React.Component {
  // render function 
  render() {
  	return (

		<div>
         <div className="row">
            <div className="col-sm-12">
               <h3><small>Inter-L Panel</small></h3>
            </div>
         </div>

         <div className="row">
            <div className="form-group col-sm-8">
                <input style={{width: 500 + "px"}} type="text" className="form-control" id="translation-input"></input>
                </div>
                <div className="col-sm-4">
                <button className="btn btn-primary btn-sm" onClick={ () => {
                    var cmd = $('#translation-input')[0].value;
                    var words = cmd.split(/(\s+)/).filter( function(e) { return e.trim().length > 0; } );
                    this.setState({ words: words});
                }}>
                Translate
                </button>
            </div>
         </div>

        <div>
            <table cassName="table table-sm" style={{width:'100%'}}>
            <tr>
            <td style={{width:100/(this.state.cols.length+1)+'%'}}><b>word</b></td>
            {this.state.cols.map( (c) => {
                return (
                        <td style={{width:100/(this.state.cols.length+1)+'%'}}><b>{c.colName}</b></td>
                );
            })}
            </tr>
            </table>
            {this.state.words.map( (w) => {
                return (
                        <WordL word={w} sbc={this.props.sbc} cols={this.state.cols}>
                        </WordL>
                );
            })}
        </div>

    </div>


  		);
  	}

    // constructor
    constructor(props) {
        super(props);
        this.state = { words : [], cols : [] };
        this.update = this.update.bind(this);
    }

    update(e, d) {
        if (d.selector == Response.ResQuery && d.record[0].selector == QueryResponse.QueryTransCols)
        {
            this.setState({cols: JSON.parse(d.record[0].record[0])});
        }
    }

    componentDidMount() {
        this.props.sbc.subscribe(this.update);
        this.props.sbc.callShoeboxCmd(new Command(Command.CmdQuery, new QueryCommand(QueryCommand.QueryTransCols)));
    }

    componentWillUnmount() {
        this.props.sbc.unsubscribe(this.update);
    } 
}

export default TabInterL; 
