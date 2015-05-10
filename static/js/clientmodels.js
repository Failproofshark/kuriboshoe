var GameTrackerClient = {}

GameTrackerClient.Game = function(initialObject) {
    this.attributes = {
        name : initialObject.name.replace(/\\/g,''),
        blurb : ((_.isNull(initialObject.blurb)) ? "" : initialObject.blurb.replace(/\\/g,'')),
        region : initialObject.region,
        hasmanual : initialObject.hasmanual,
        hasbox : initialObject.hasbox,
        notes :  ((_.isNull(initialObject.notes)) ? "" : initialObject.notes.replace(/\\/g,'')),
        quantity : initialObject.quantity,
        systemname : "",
        genres: [],
        companies: []
    };
    this.attributes.systemname = _.result(_.find(systems, {id: initialObject.systemid}), "name").replace(/\\/g,'');
    
    var ensureArray = function(item) {
        var returnValue = _.isArray(item) ? item : [item];
        return returnValue;
    };
    /* Bit of a symbolic manipulation trick here ;). Given that the initialObject and global namespace
     * use the same name for genres and companies we simply pass a string, use eval to get the object
     * from global namespace and still use it to reference the attribute we want in the initialObject namespace.
     */
    var getRelatedNames = function(collectionName) {
        var singularName = (collectionName === "genres") ? "genreId" : "companyId";
        return _.map(_.pluck(_.filter(eval(collectionName), function(item) {
            return _.contains(_.pluck(ensureArray(initialObject[collectionName]), singularName), item.id);
        }),
                       "name"), function(item) { return item.replace(/\\/g,''); });
    };
    
    this.attributes.genres = getRelatedNames("genres");
    this.attributes.companies = getRelatedNames("companies");

};
