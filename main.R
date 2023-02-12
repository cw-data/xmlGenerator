# a minimal workflow
library(xmlGenerator)

xmlGenerator::load_example_data()
xmlGenerator::loadData(example_data)
xmlGenerator::buildXML(record_list, write = FALSE)
