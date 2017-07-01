//#include "dev.h"
#include "quanteda.h"
#include <bitset>
using namespace quanteda;


void counts(Text text, 
            MapNgrams &counts_seq,
            const std::vector<unsigned int> &sizes,
            const bool &nested){
    
    if (text.size() == 0) return; // do nothing with empty text
    text.push_back(0); // add padding to include last words

    // Collect sequence of specified types
    std::size_t len_text = text.size();
    for (std::size_t size : sizes) {
        for (std::size_t i = 0; i <= len_text; i++) {
            //Rcout << "Size" << size << "\n";
            if (i + size < len_text) {
                if (std::find(text.begin() + i, text.begin() + i + size, 0) == text.begin() + i + size) {
                    // dev::print_ngram(text_sub);
                    Text text_sub(text.begin() + i, text.begin() + i + size);
                    counts_seq[text_sub]++;
                }
                if (!nested) {
                    i += size - 1;
                    // Rcout << "Skip" << i + size - 1 << "\n";
                }
            }
        }
    }
}


struct counts_mt2 : public Worker{
    
    Texts texts;
    MapNgrams &counts_seq;
    const std::vector<unsigned int> &sizes;
    const bool &nested;
    
    counts_mt2(Texts texts_, MapNgrams &counts_seq_, std::vector<unsigned int> &sizes_, const bool &nested_):
        texts(texts_), counts_seq(counts_seq_), sizes(sizes_), nested(nested_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++){
            counts(texts[h], counts_seq, sizes, nested);
        }
    }
};


// [[Rcpp::export]]
DataFrame qatd_cpp_count2(const List &texts_,
                          const CharacterVector &types_,
                          const IntegerVector sizes_,
                          const bool nested){
    
    Texts texts = as< Texts >(texts_);
    std::vector<unsigned int> sizes = as< std::vector<unsigned int> >(sizes_);
    MapNgrams counts_seq;
    
#if QUANTEDA_USE_TBB
    counts_mt2 counts_mt2(texts, counts_seq, sizes, nested);
    parallelFor(0, texts.size(), counts_mt2);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        counts(texts[h], counts_seq, sizes, nested);
    }
#endif
    
    std::size_t len = counts_seq.size();
    VecNgrams seqs;
    IntParams cs, ns;
    seqs.reserve(len);
    cs.reserve(len);
    
    for (auto it = counts_seq.begin(); it != counts_seq.end(); ++it) {
        seqs.push_back(it -> first);
        cs.push_back(it -> second);
    }
    
    CharacterVector seqs_(seqs.size());
    for (std::size_t i = 0; i < seqs.size(); i++) {
        seqs_[i] = join(seqs[i], types_, " ");
    }
    
    DataFrame output_ = DataFrame::create(_["collocation"] = seqs_,
                                          _["count"] = as<IntegerVector>(wrap(cs)),
                                          _["stringsAsFactors"] = false);
    return output_;
    
}



/***R
require(quanteda)
toks <- tokens(data_corpus_inaugural)
#load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
#toks <- data_tokens_guardian
out <- qatd_cpp_count2(toks, attr(toks, 'type'), 1:3, TRUE)
head(out)


out_prev <- NULL
out <- NULL
for (i in 1:100) {
    cat(i, "")
    out <- qatd_cpp_count2(toks, attr(toks, 'type'), 1:3, TRUE)
    out <- out[order(out$collocation),]
    if (!is.null(out) && !is.null(out_prev)) {
        if (!all(out$count == out_prev$count) ||
            !all(out$collocation == out_prev$collocation)) {
        #if(!identical(out, out_prev)) {
            print("Not the same as previous run")
        }
    }
    out_prev <- out
}

out1 <- qatd_cpp_count2(toks, attr(toks, 'type'), 1:3, TRUE)
out2 <- qatd_cpp_count2(toks, attr(toks, 'type'), 1:3, TRUE)
out1 <- out1[order(out1$collocation),]
out2 <- out2[order(out2$collocation),]

identical(out1, out2)
all(out1$count == out2$count)
all(out1$collocation == out2$collocation)


*/
