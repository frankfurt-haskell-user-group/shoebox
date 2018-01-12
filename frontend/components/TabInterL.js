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

var colsTable = function (cols) {
    if (typeof(cols) == "string") {
        return (
                <div>{cols}</div>
        );
    }
    if (typeof(cols) == "object") {
    return (
            <table className="table table-bordered table-sm" style={{marginBottom:'0px', padding:'0px'}}>
              <tr>
                <td>
                  {Object.keys(cols)[0]}
                </td>
                <td>
                  <table className="table table-sm" style={{marginBottom:'0px', padding:'0px'}}>
                  {Object.values(cols)[0].map(
                      (c) => {
                          return (
                                    <tr>
                                      <td>
                                  <table className="table table-sm" style={{marginBottom:'0px', padding:'0px'}}>
                                  {c.map(
                                      (d) => {
                                          if (typeof(d) == "string" || typeof(d) == "object") {
                                          return (
                                                  <tr><td>
                                                  {colsTable(d)}
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
                  )}
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

class WordL extends React.Component {

    // render function
    render () {
        return (
            <div>
            {colsTable(this.state.result)}
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
                console.log("query answer: ", d.record[0].record[1]);
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
            console.log("cols: ", this.state.cols);
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
