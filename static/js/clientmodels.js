var GameTrackerClient = {}

GameTrackerClient.Game = function(initialObject) {
    this.attributes = {
        name : initialObject.name,
        blurb : initialObject.blurb,
        region : initialObject.region,
        hasmanual : initialObject.hasmanual,
        hasbox : initialObject.hasbox,
        notes : initialObject.notes,
        quantity : initialObject.quantity,
        systemname : "",
        genres: [],
        companies: []
    };
    this.attributes.systemname = _.result(_.find(systems, {id: initialObject.systemid}), "name");
    
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
        return _.pluck(_.filter(eval(collectionName), function(item) {
            return _.contains(_.pluck(ensureArray(initialObject[collectionName]), singularName), item.id);
        }),
                       "name");
    };
    
    this.attributes.genres = getRelatedNames("genres");
    this.attributes.companies = getRelatedNames("companies");

};
