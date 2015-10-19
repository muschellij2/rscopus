
#' @title Replace non-ASCII characters
#'
#' @description Replaces non-ASCII chracters from a last or first name
#' @param string Character vector of values to be replaced
#' @export
#' @seealso Taken from \url{http://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding}
#' @return Chracter vector
replace_non_ascii <- function(string){
  #   unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
  #                         'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
  #                         'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
  #                         'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
  #                         'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  raws = structure(list(S = as.raw(c(0xc5, 0xa0)), s = as.raw(c(0xc5,
                                                                0xa1)), Z = as.raw(c(0xc5, 0xbd)), z = as.raw(c(0xc5, 0xbe)),
                        A = as.raw(c(0xc3, 0x80)), A = as.raw(c(0xc3, 0x81)), A = as.raw(c(0xc3,
                                                                                           0x82)), A = as.raw(c(0xc3, 0x83)), A = as.raw(c(0xc3, 0x84
                                                                                           )), A = as.raw(c(0xc3, 0x85)), A = as.raw(c(0xc3, 0x86)),
                        C = as.raw(c(0xc3, 0x87)), E = as.raw(c(0xc3, 0x88)), E = as.raw(c(0xc3,
                                                                                           0x89)), E = as.raw(c(0xc3, 0x8a)), E = as.raw(c(0xc3, 0x8b
                                                                                           )), I = as.raw(c(0xc3, 0x8c)), I = as.raw(c(0xc3, 0x8d)),
                        I = as.raw(c(0xc3, 0x8e)), I = as.raw(c(0xc3, 0x8f)), N = as.raw(c(0xc3,
                                                                                           0x91)), O = as.raw(c(0xc3, 0x92)), O = as.raw(c(0xc3, 0x93
                                                                                           )), O = as.raw(c(0xc3, 0x94)), O = as.raw(c(0xc3, 0x95)),
                        O = as.raw(c(0xc3, 0x96)), O = as.raw(c(0xc3, 0x98)), U = as.raw(c(0xc3,
                                                                                           0x99)), U = as.raw(c(0xc3, 0x9a)), U = as.raw(c(0xc3, 0x9b
                                                                                           )), U = as.raw(c(0xc3, 0x9c)), Y = as.raw(c(0xc3, 0x9d)),
                        B = as.raw(c(0xc3, 0x9e)), Ss = as.raw(c(0xc3, 0x9f)), a = as.raw(c(0xc3,
                                                                                            0xa0)), a = as.raw(c(0xc3, 0xa1)), a = as.raw(c(0xc3, 0xa2
                                                                                            )), a = as.raw(c(0xc3, 0xa3)), a = as.raw(c(0xc3, 0xa4)),
                        a = as.raw(c(0xc3, 0xa5)), a = as.raw(c(0xc3, 0xa6)), c = as.raw(c(0xc3,
                                                                                           0xa7)), e = as.raw(c(0xc3, 0xa8)), e = as.raw(c(0xc3, 0xa9
                                                                                           )), e = as.raw(c(0xc3, 0xaa)), e = as.raw(c(0xc3, 0xab)),
                        i = as.raw(c(0xc3, 0xac)), i = as.raw(c(0xc3, 0xad)), i = as.raw(c(0xc3,
                                                                                           0xae)), i = as.raw(c(0xc3, 0xaf)), o = as.raw(c(0xc3, 0xb0
                                                                                           )), n = as.raw(c(0xc3, 0xb1)), o = as.raw(c(0xc3, 0xb2)),
                        o = as.raw(c(0xc3, 0xb3)), o = as.raw(c(0xc3, 0xb4)), o = as.raw(c(0xc3,
                                                                                           0xb5)), o = as.raw(c(0xc3, 0xb6)), o = as.raw(c(0xc3, 0xb8
                                                                                           )), u = as.raw(c(0xc3, 0xb9)), u = as.raw(c(0xc3, 0xba)),
                        u = as.raw(c(0xc3, 0xbb)), y = as.raw(c(0xc3, 0xbd)), y = as.raw(c(0xc3,
                                                                                           0xbd)), b = as.raw(c(0xc3, 0xbe)), y = as.raw(c(0xc3, 0xbf
                                                                                           ))), .Names = c("S", "s", "Z", "z", "A", "A", "A", "A", "A",
                                                                                                           "A", "A", "C", "E", "E", "E", "E", "I", "I", "I", "I", "N", "O",
                                                                                                           "O", "O", "O", "O", "O", "U", "U", "U", "U", "Y", "B", "Ss",
                                                                                                           "a", "a", "a", "a", "a", "a", "a", "c", "e", "e", "e", "e", "i",
                                                                                                           "i", "i", "i", "o", "n", "o", "o", "o", "o", "o", "o", "u", "u",
                                                                                                           "u", "y", "y", "b", "y"))

  unwanted_array = names(raws)
  unwanted_array_names = sapply(raws, rawToChar)
  names(unwanted_array) =  unwanted_array_names

  chartr(paste(names(unwanted_array), collapse=''),
         paste(unwanted_array, collapse=''),
         string)
}