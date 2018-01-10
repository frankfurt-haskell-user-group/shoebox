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

class WordL extends React.Component {

    // render function
    render () {
        return (

        <div>
        <h2>
            {this.props.word}
        </h2>
            <p>{this.state.result}</p>
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
        if (d.selector == Response.TestAnswer && d.record[0].split(" ")[0] == this.state.id)
        {
            var ns = d.record[0].split(" ").slice(1).join("");
            if (ns != this.state.result) {
                this.setState( { result : ns});
            }
        }
    }

    componentDidMount() {
        this.props.sbc.subscribe(this.update);
        this.props.sbc.callShoeboxCmd(new Command(Command.RunTest, this.state.id + " " + this.props.word ));
    }

    componentDidUpdate() {
        this.props.sbc.callShoeboxCmd(new Command(Command.RunTest, this.state.id + " " + this.props.word ));
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
                        <WordL word={w} sbc={this.props.sbc}>
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
        this.state = { words : []};
        this.update = this.update.bind(this);
    }

    update(e, d) {
        if (d.selector == Response.ResQuery && d.record[0].selector == QueryResponse.DbQuery)
        {
        }
    }

    componentDidMount() {
        this.props.sbc.subscribe(this.update);
    }

    componentWillUnmount() {
        this.props.sbc.unsubscribe(this.update);
    } 
}

export default TabInterL; 
