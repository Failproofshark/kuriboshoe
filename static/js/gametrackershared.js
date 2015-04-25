var GameTrackerShared = {};

GameTrackerShared.TrackerForm = function(fields) {
    this.fields = fields;
    this.populateForm = function(object) {
        var self = this;
        if (object.attributes) {
            _.map(object.attributes, function(attributeValue, attributeKey) {
                if (attributeKey !== "id") {
                    self.fields[attributeKey](attributeValue);
                }
            });
        } else {
            _.map(object, function(value, key) {
                if (key !== "id") {
                    self.fields[key](value);
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
        return _.mapValues(this.fields, function(field) {
            var returnValue = field();
            if (_.isBoolean(returnValue)) {
                returnValue = Number(returnValue);
            }
            return returnValue;
        });
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
