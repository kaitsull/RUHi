#unlocked mFISH class

#create mFISH object class
setClass(Class='mFISH',
                  slots=c('rawData'='data.frame', 'filteredData'='data.frame',
                          'metaData'='data.frame', 'attributes'='list'))



