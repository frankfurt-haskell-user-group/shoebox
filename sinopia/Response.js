import CBOR from '../other_libs/cbor';

class CborStructItem {

    constructor(...args) {
       this.setValue(...args);
    }

    setValue(...args) {
       this.record = args;
    }

    fromCBOR (ser_data) {
       var json_data = CBOR.decode(ser_data);
       this.fromData(json_data);
    }

    toCBOR () {
       var data = this.toData();
       return CBOR.encode(data);
    }

    toData () {
       return this.record;
    }

    fromData (json_data) {
       this.setValue(json_data);
    }
}

class CborEnumItem extends CborStructItem {
    setValue(...args) {
       if (arguments.length > 0) {
         this.selector = arguments[0];
         this.record = arguments.slice(0,1);
       }
    }

    toData () {
       return [this.selector, ...this.record];
    }
}

// response to file operations
class FileResponse extends CborEnumItem {
}

FileResponse.CurrentDBChanged = 0;   // the DB in use just changed
FileResponse.AvailableDBs = 1;   // list of DB's on disk
FileResponse.OpenedDB = 2;   // just opened the DB
FileResponse.CreatedDB = 3;   // just created the DB
FileResponse.DeletedDB = 4;   // just deleted the DB
FileResponse.SavedDB = 5;   // just saved the DB

// response to database queries
class QueryResponse extends CborEnumItem {
}

QueryResponse.DbInfo = 0;   // detailed DB info as JSON text
QueryResponse.DbQuery = 1;   // query answer as JSON text

// response to commands
class Response extends CborEnumItem {
    toData () {
       var arr_in = this.record.slice(); var arr_out = [];
       if (this.selector == 0) {
       }
       if (this.selector == 1) {
            arr_out.push(arr_in.shift().toData());
       }
       if (this.selector == 2) {
            arr_out.push(arr_in.shift().toData());
       }
       return [this.selector, ...arr_out];
    }

     fromData (json_data) {
       var arr_in = json_data.slice(0, 1); var arr_out = [];
       if (json_data[0] == 0) {
       }
       if (json_data[0] == 1) {
            arr_out.push((new FileResponse()).fromData(arr_in.shift());
       }
       if (json_data[0] == 2) {
            arr_out.push((new QueryResponse()).fromData(arr_in.shift());
       }
       this.selector = json_data[0];
       this.record = arr_out;
       }
}

Response.NoResponse = 0;   // response to NoOP command (is this needed?)
Response.ResFr = 1;   // response to a file command
Response.ResQuery = 2;   // response to a query command

