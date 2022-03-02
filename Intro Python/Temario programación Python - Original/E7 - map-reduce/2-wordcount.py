from mrjob.job import MRJob

class MRCharCount(MRJob):

    def mapper(self, _, line):
        for w in line.split():
            yield w, 1
                
    def reducer(self, key, values):
        yield key, sum(values)

if __name__ == '__main__':
    MRCharCount.run()
