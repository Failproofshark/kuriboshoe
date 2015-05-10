var GameTrackerShared = {};

GameTrackerShared.TrackerForm = function(fields) {
    this.fields = fields;
    this.populateForm = function(object) {
        var self = this;
        if (object.attributes) {
            //Used for data that's kept in a model (user selects from a drop down)
            _.map(object.attributes, function(attributeValue, attributeKey) {
                if (attributeKey !== "id") {
                    var actualValue = (_.isString(attributeValue)) ? attributeValue.replace(/\\/g,'') : attributeValue;
                    // We need to strip out any slashes recorded in the db entry
                    self.fields[attributeKey](actualValue);
                }
            });
        } else {
            //Mainly used for bootstrapped data and data that comes in from the server (used mainly with games)
            _.map(object, function(value, key) {
                var actualValue = (_.isString(value)) ? value.replace(/\\/g, '') : value;
                if (key !== "id") {
                    self.fields[key](actualValue);
                }
            });
        }
    };
    this.clearForm = _.forEach.bind(this, this.fields, function(input) {
        if (_.isString(input())) {
            input("");
        } else if (_.isArray(input())) {
            input([]);
        } else if (_.isBoolean(input())){
            input(false);
        } else {
            input(null);
        }
    });
    this.returnFields = function() {
        return _.omit(_.mapValues(this.fields, function(field) {
            var returnValue = field();
            if (_.isBoolean(returnValue)) {
                returnValue = Number(returnValue);
            }
            return returnValue;
        }), _.isNull);
    };
    this.submitHandlers = {};
    /* This will probably be refactored out in the future given the only thing that has a search is the game form
     * To keep things from complaining about a missing key we add an empty function here
     */
    this.submitHandlers.search = function() { /*empty*/ };
    this.getSubmitHandler = function(state) {
        return this.submitHandlers[state];
    };

};
