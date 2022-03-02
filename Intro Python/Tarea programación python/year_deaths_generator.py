def read_data_mr(file, year):
    """
    A partir de la lectura del fichero csv generamos
    un fichero con los datos filtrados para usar la funcion
    map reduce. Se genra './deaths-filter-by-year.txt'.
    
    Return
    ------
        
    Parameters
    ----------
    file : str -- archivo ("./file.csv")
    year : int -- año
    
    Exceptions
    ----------
    
    Example
    -------
    >>> read_data_mr("./annual-number-of-deaths-by-cause.csv", 2012)
        deaths-filter-by-year.txt
        
    """
    with open('./deaths-filter-by-year.txt','w') as fw:
        with open(file, 'r') as f:
            for line in f:
                fields = line.split(',')
                if (fields[2] == str(year)):
                    fw.write(line)
        #fw.close()
