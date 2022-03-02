// delete
db.parados.deleteMany({$and: [ {"Nombre":/.*Total Nacional.*/}] })


// a. listamos todos los datos y el numero de documentos
db.parados.find({}).count()

db.parados.find({})


// b. sectores de desempelo 
db.parados.find({ "CCAA": /.*Madrid*./ }, { "Sector": 1, "T3_Escala": 1, "T3_Unidad": 1, _id: 0 })


// c. total de trimestres en el muestreo de datos
db.parados.aggregate([
    { $match: { "CCAA": /.*Madrid*./ } },
    {
        $project:
        {
            _id: 0,
            initDate: { $arrayElemAt: ["$Data.Fecha", 0] },
            lastDate: { $arrayElemAt: ["$Data.Fecha", -1] }
        }
    }
]).limit(1)


// d. Lista las 5 comunidades que mas desempleados tienen acutalmente
db.parados.find({
    $and: [
        { "Sector": "Total CNAE" },
    ]
}, { "CCAA": 1, _id: 0 }).sort({ "Data.0.Valor": -1 }).limit(5)



// e. total de parados el primer trimestre antes y despues de la pandemia
db.parados.aggregate([
    {
        $match: {
            "Sector": "Total CNAE"

        }
    },
    {
        $project:
        {
            _id: 0,
            "CCAA": 1,
            "Sector": 1,
            firstTri: { $arrayElemAt: ["$Data.Valor", 0] },
            lastTri: { $arrayElemAt: ["$Data.Valor", 2] },
        }
    },
    { $group: { _id: "Total desempeados España 2020", totalFirstTri: { $sum: "$firstTri" }, totalLastTri: { $sum: "$lastTri" } }
])


// f. Parados en murcia por sector
db.parados.aggregate([
    {
        $match: {

            "CCAA": /.*Murcia*./

        }
    },
    {
        $project:
        {
            _id: 0,
            "CCAA": 1,
            "Sector": 1,
            Data: { $arrayElemAt: ["$Data", 0] },
        }
    },
]).sort({ _id: -1 })


// g. Sectores que mas se han visto afectados por la pandemia
// inicio de la primera ola
db.parados.aggregate([
    {
        $project:
        {
            _id: 0,
            "CCAA": 1,
            "Sector": 1,
            Data: { $arrayElemAt: ["$Data", 1] },
        }
    },
    { $group: { _id: "$Sector", totalFirstTri: { $sum: "$Data.Valor" } } }
     {
        $match: {

            _id: { $not: /Total CNAE/ }

        }
    },
]).sort({ _id: -1 })

// inicio de la segunda ola
db.parados.aggregate([
    {
        $project:
        {
            _id: 0,
            "CCAA": 1,
            "Sector": 1,
            Data: { $arrayElemAt: ["$Data", 0] },
        }
    },
    { $group: { _id: "$Sector", totalThidTri: { $sum: "$Data.Valor" } } }
    {
        $match: {

            _id: { $not: /Total CNAE/ }

        }
    },
]).sort({ _id: -1 })


// h. total de parados al inicio de la crisis 2008 (Primer trimestre)
db.parados.aggregate([
    {
        $match: {

            "Sector": "Total CNAE"

        }
    },
    {
        $project:
        {
            _id: 0,
            "CCAA": 1,
            "Sector": 1,
            firstTri: { $arrayElemAt: ["$Data.Valor", -1] },
        }
    },
    { $group: { _id: "Total desempeados España 2008", totalFirstTri: { $sum: "$firstTri" } } }
])



//  i. total de parados tras la crisis 2015 (Primer trimestre)
db.parados.aggregate([
    { $unwind: "$Data" },
    {
        $match: {
            $and: [

                { "Sector": "Total CNAE" }
                { "Data.Anyo": 2015 }
                { "Data.T3_Periodo": "T1" }
            ]
        }

    }
    { $group: { _id: "Total desempeados España 2018", totalFirstTri: { $sum: "$Data.Valor" } } }
])


//  j. el año que mas parados ha tenido 
var aux1 = db.parados.aggregate([
    { $unwind: "$Data" },
    {
        $match: {
            $and: [

                { "Sector": "Total CNAE" }
            ]
        }

    }
    { $group: { _id: "$Data.Fecha", totalAnual: { $sum: "$Data.Valor" } } }
    { $group: { _id: "Máximo parados", maxAnual: { $max: "$totalAnual" } } }
])

//aux1['maxAnual']
getMaxAnual = function(doc) { return doc.maxAnual; }
var maxAnualAux = aux1.map(getMaxAnual);
//maxAnualAux;


db.parados.aggregate([
    { $unwind: "$Data" },
    {
        $match: {
            $and: [

                { "Sector": "Total CNAE" },
                { "Data.T3_Periodo": "T1" },
            ]
        }
    { $group: { _id: "$Data.Fecha", totalAnual: { $sum: "$Data.Valor" } } }
    { $match: { "totalAnual": maxAnualAux[0] } }// 6278.1 }}
    { $project: { "totalAnual": 1 } }
            }
])



//  k. el año que menos parados ha tenido 
var aux2 = db.parados.aggregate([
    { $unwind: "$Data" },
    {
        $match: {
            $and: [

                { "Sector": "Total CNAE" }
            ]
        }

    }
    { $group: { _id: "$Data.Fecha", totalAnual: { $sum: "$Data.Valor" } } }
    { $group: { _id: "Máximo parados", minAnual: { $min: "$totalAnual" } } }
])

//aux2['minAnual']
getMinAnual = function(doc) { return doc.minAnual; }
var minAnualAux = aux2.map(getMinAnual);
//minAnualAux;

db.parados.aggregate([
    { $unwind: "$Data" },
    {
        $match: {
            $and: [

                { "Sector": "Total CNAE" },
                { "Data.T3_Periodo": "T1" },
            ]
        }
    { $group: { _id: "$Data.Fecha", totalAnual: { $sum: "$Data.Valor" } } }
    { $match: { "totalAnual": minAnualAux[0] } }//2190.6 }}
    { $project: { "totalAnual": 1 } }
            }
])



//12.

