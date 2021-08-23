from mrjob.job import MRJob
from mrjob.step import MRStep

class Total_Amount_Sorted(MRJob):
    def steps(self):
        return [
            MRStep(mapper=self.mapper_total_amount_per_customer,
                   reducer=self.reducer_mapper_total_amount_per_customer),
            MRStep(mapper=self.mapper_sort_values,
                   reducer = self.reducer_sort_values)
        ]
#Find the Total Amount Spent per Customer
        
    def mapper_total_amount_per_customer(self, _, line):
        (Customer_ID, Item_ID, TotalAmount_Spent) = line.split(',')
        yield Customer_ID, float(TotalAmount_Spent)
        
    def reducer_mapper_total_amount_per_customer(self, Customer_ID, TotalAmount_Spent):
        yield Customer_ID, sum(TotalAmount_Spent)

#Sort the values
        
    def mapper_sort_values(self, Customer_ID, AmountSpent): #want to sort
        yield '%04.02f'%float(AmountSpent), Customer_ID

    def reducer_sort_values(self, AmountSpent, CustomerIDs):
        for Customer_ID in Customer_IDs:
            yield AmountSpent, Customer_ID

if __name__ == '__main__':
    Total_Amount_Sorted.run()
    
# Execution:
#!python M3_MapReduce_Assignment.py buad5132-m3-individual-dataset.csv > M3_Total_Amount_Sorted.txt
