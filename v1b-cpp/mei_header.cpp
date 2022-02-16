/* meiHeader.cc
 * Andrew Cashner, 2021/12/13
 *
 * Extract the \header{} from a Lilypond file and translate it to an MEI
 * meiHead element.
 */

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <map>

#include <mei/mei.h>
#include <mei/header.h>
#include <mei/shared.h>
#include <mei/xmlexport.h>

/* CONSTANTS */
/* The name of this program */
const std::string kProgramName = "ly2mei";

/* Whitespace characters */
const std::string kWhitespace = " \r\n\t\f\v";

/* INPUT */
/* Read the contents of a file into a string. */
std::string file_to_string(std::string filename) {
    std::string output_str;
    std::ifstream infile(filename);
    if (infile) {
        std::ostringstream file_str;
        file_str << infile.rdbuf();
        output_str = file_str.str();
    }
    return output_str;
}

/* TRIM WHITESPACE */

/* Trim from left side of string */
std::string trim_left(std::string source) {
    std::string output_str = source;
    size_t end_index = source.find_first_not_of(kWhitespace);
    if (end_index != std::string::npos) {
        output_str = source.erase(0, end_index);
    }
    return output_str;
}

/* Trim from right side of string */
std::string trim_right(std::string source) {
    std::string output_str = source;
    size_t start_index = source.find_last_not_of(kWhitespace);
    if (start_index != std::string::npos) {
        output_str = source.erase(start_index + 1);
    } else {
        output_str = "";
    }
    return output_str;
}

/* Trim whitespace from both ends of a string. */
std::string trim(std::string source) {
    return (trim_right(trim_left(source)));
}

/* EXTRACT PORTIONS OF STRINGS */

/* Copy the portion of a string within balanced delimiters such as curly
 * brackets, including any nested delimited strings. Delimiters are included
 * in the output string. */
std::string balanced_delimiter_substring(std::string source,
        char start_delim, char end_delim) {
    
    std::string output_str = "";
    size_t start_index = source.find(start_delim);

    if (start_index != std::string::npos) {
        int brace_level = 0;
        size_t end_index = start_index;
        for (auto& c : source) {
            if (c == start_delim) {
                ++brace_level;
            } else if (c == end_delim) {
                --brace_level;
                if (brace_level == 0) {
                    size_t expr_length = end_index - start_index + 1;
                    output_str = source.substr(start_index, expr_length);
                    break;
                }
            }
            ++end_index;
        }
    }
    return output_str;
}

/* Find substring delimited by curly braces. */
std::string brace_delimited_substring(std::string source) {
    return (balanced_delimiter_substring(source, '{', '}'));
}

/* Find the position starting after the given substring */
size_t find_first_after_substring(std::string source, std::string substring) {
    size_t index = source.find(substring);
    if (index != std::string::npos) {
        index += substring.length();
    }
    return index;
}

/* Given a Lilypond command (e.g., \markup {}) extract its
 * curly-brace-delimited argument. */
std::string ly_brace_argument(std::string source, std::string command) {
    std::string arg = "";
    size_t start_index = find_first_after_substring(source, command);
    if (start_index != std::string::npos) {
        arg = brace_delimited_substring(
                source.substr(start_index, std::string::npos));
    }
    return arg;
}

/* Delete a prefix from a string; return the portion of the string starting
 * after the last character of the prefix. If no prefix found, return the
 * original string. */
std::string drop_before(std::string source, std::string prefix) {
    std::string output_str = source;
    size_t index = find_first_after_substring(source, prefix);
    if (index != std::string::npos) {
        output_str = source.erase(0, index);
    }
    return output_str;
}

/* Delete a suffix from a string; return the portion of the string ending 
 * before the first character of the suffix. If no suffix found, return the
 * original string. */
std::string drop_after(std::string source, std::string suffix) {
    std::string output_str = source;
    size_t index = source.find(suffix);
    if (index != std::string::npos) {
        output_str = source.erase(index);
    }
    return output_str;
}

/* Copy the portion of a string between quotation marks ('"'). */
std::string quoted_substring(std::string source) {
    std::string output_str = "";
    if (source.find('"') != std::string::npos) {
        std::string after_quote = drop_before(source, "\"");
        output_str = drop_after(after_quote, "\"");
    }
    return output_str;
}

/* Find all quoted substrings and return their contents in a single string,
 * with quotation marks removed, and substrings separated by a space. */
std::string concat_quoted_substrings(std::string source) {
    std::string output_str = "";
    while (source.find('"') != std::string::npos) {
        std::string this_substring = quoted_substring(source);
        if (!this_substring.empty()) {
            output_str += " " + this_substring;
            source = drop_before(source, "\"" + this_substring + "\""); 
        }
    }
    return trim(output_str);
}

/* Does the string start with a given substring? */
bool starts_with(std::string source, std::string start_string) {
    return (source.find(start_string) == 0);
}

/* DICTIONARY FOR HEADER */
using dictionary = std::map<std::string, std::string>;


/* Find a Lilypond \header{} and return a dictionary with the keys and values
 * that were defined within it. 
 *
 * A key must begin on a new line and be followed by " = " and the value. 
 *
 * The value may either be a single string in quotation marks, or a \markup{}
 * expression with the argument enclosed in curly braces. In that case, the
 * value will be the space-separated concatenation of all quoted strings found
 * inside the markup expression.
 * */
dictionary header_to_dict(std::string source) {
    dictionary header_dict;
    std::istringstream source_stream(source);
    std::string this_line;
    size_t this_line_index = 0;
    while (std::getline(source_stream, this_line)) {
        size_t start_index = this_line.find(" = ");
        if (start_index != std::string::npos) {
            std::string key = trim(this_line.substr(0, start_index));
            std::string value = this_line.substr(start_index + 3, std::string::npos);

            std::string markup_cmd = "\\markup ";
            if (starts_with(trim_left(value), markup_cmd)) {
                std::string test = source.substr(this_line_index, std::string::npos);
                value = concat_quoted_substrings(ly_brace_argument(test, markup_cmd));

                this_line_index += find_first_after_substring(this_line, markup_cmd) 
                                    + value.length();
            } else {
                value = quoted_substring(value);
                this_line_index += this_line.length();
            }
            header_dict.emplace(key, value);
        }
    }
    return header_dict;
}

/* Find the Lilypond header expression and parse its contents into a
 * dictionary of keys and values. */
dictionary extract_header_data(std::string source) {
    std::string header = ly_brace_argument(source, "\\header ");
    dictionary header_dict = header_to_dict(header);
    return header_dict;
}

/* WRITING THE MEI HEADER (meiHead) */

/* Within titleStmt, the title or subtitle */
mei::TitleStmt* mei_titleStmt_add_title(mei::TitleStmt *titleStmt, 
        std::string key, std::string value) {
    if (titleStmt) {
        mei::MeiElement *title = new mei::MeiElement("title");

        std::string type = "main";
        if (key == "subtitle") {
            type = "subtitle";
        }
        mei::MeiAttribute *title_type = new mei::MeiAttribute("type", type);
        title->addAttribute(title_type);
        
        title->setValue(value);
        titleStmt->addChild(title);
    }
    return titleStmt;
}

/* mei/meiHead/titleStmt/title[@type="main"] */

mei::TitleStmt* mei_titleStmt_add_maintitle(mei::TitleStmt *titleStmt, 
        std::string value) {
    titleStmt = mei_titleStmt_add_title(titleStmt, "title", value);
    return titleStmt;
}

/* mei/meiHead/titleStmt/title[@type="subtitle"] */
mei::TitleStmt* mei_titleStmt_add_subtitle(mei::TitleStmt *titleStmt, 
        std::string value) {
    titleStmt = mei_titleStmt_add_title(titleStmt, "subtitle", value);
    return titleStmt;
}

/* mei/meiHead/respStmt elements */
mei::RespStmt* mei_respStmt_add_element(mei::RespStmt *respStmt, 
    std::string key, std::string value) {
    if (respStmt) {
        mei::MeiElement *element = new mei::MeiElement(key);
        element->setValue(value);
        respStmt->addChild(element);
    }
    return respStmt;
}

/* mei/meiHead/respStmt/composer */
mei::RespStmt* mei_respStmt_add_composer(mei::RespStmt *respStmt, 
    std::string value) {
    respStmt = mei_respStmt_add_element(respStmt, "composer", value);
    return respStmt;
}

/* Dates to be appended to mei/meiHead/respStmt/composer (from "dates" in
 * Lilypond (lirio) header */
mei::RespStmt* mei_respStmt_add_dates(mei::RespStmt *respStmt, 
    std::string value) {
    if (respStmt) {
        mei::MeiElement *composer = respStmt->getChildrenByName("composer")[0];
        if (composer) {
            std::string composer_str = composer->getValue();
            composer->setValue(composer_str + " " + value);
        }
    }
    return respStmt;
}

/* mei/meiHead/respStmt/lyricist (from "poet" in Lilypond header) */
mei::RespStmt* mei_respStmt_add_lyricist(mei::RespStmt *respStmt, 
    std::string value) {
    respStmt = mei_respStmt_add_element(respStmt, "lyricist", value);
    return respStmt;
}

/* mei/meiHead/respStmt/editor */
mei::RespStmt* mei_respStmt_add_editor(mei::RespStmt *respStmt, 
    std::string value) {
    respStmt = mei_respStmt_add_element(respStmt, "editor", value);
    return respStmt;
}

/* mei/meiHead/pubStmt/availability ("copyright") */
mei::PubStmt* mei_pubStmt_add_availability(mei::PubStmt *pubStmt, 
    std::string value) {
    if (pubStmt) {
        mei::MeiElement *element = new mei::MeiElement("availability");
        element->setValue(value);
        pubStmt->addChild(element);
    }
    return pubStmt;
}

/* mei/meiHead/sourceDesc/source ("source") */
mei::SourceDesc* mei_sourceDesc_add_source(mei::SourceDesc *sourceDesc, 
        std::string value) {
    if (sourceDesc) {
        mei::MeiElement *element = new mei::MeiElement("source");
        element->setValue(value);
        sourceDesc->addChild(element);
    }
    return sourceDesc;
}

/* mei/meiHead/encodingDesc/appInfo/application */
mei::EncodingDesc* mei_encoding_Desc_add_app(mei::EncodingDesc *encodingDesc, 
        std::string value) {
    if (encodingDesc) {
        mei::MeiElement *info = new mei::MeiElement("appInfo");
        mei::MeiElement *app  = new mei::MeiElement("application");
        app->setValue(value);
       
        info->addChild(app);
        encodingDesc->addChild(info);
    }
    return encodingDesc;
}

 
/* Create the MEI meiHead element from the header dictionary. */
// TODO enforce specific order of header fields (e.g., main title before
// subtitle)?
mei::MeiHead* create_header(mei::MeiHead *meiHead, dictionary dict) {
   
    if (meiHead) {
        mei::TitleStmt *titleStmt       = new mei::TitleStmt();
        mei::RespStmt *respStmt         = new mei::RespStmt();
        mei::PubStmt *pubStmt           = new mei::PubStmt();
        mei::SourceDesc *sourceDesc     = new mei::SourceDesc();
        mei::EncodingDesc *encodingDesc = new mei::EncodingDesc();
     
        for (auto& [key, value] : dict) {
            if (key == "title") {
                titleStmt = mei_titleStmt_add_maintitle(titleStmt, value);
            } else if (key == "subtitle") {
                titleStmt = mei_titleStmt_add_subtitle(titleStmt, value);
            } else if (key == "composer") {
                respStmt = mei_respStmt_add_composer(respStmt, value);
            } else if (key == "dates") {
                respStmt = mei_respStmt_add_dates(respStmt, value);
            } else if (key == "poet") {
                respStmt = mei_respStmt_add_lyricist(respStmt, value);
            } else if (key == "editor") {
                respStmt = mei_respStmt_add_editor(respStmt, value);
            } else if (key == "copyright") {
                pubStmt = mei_pubStmt_add_availability(pubStmt, value);
            } else if (key == "source") {
                sourceDesc = mei_sourceDesc_add_source(sourceDesc, value);
            }
        }
        encodingDesc = mei_encoding_Desc_add_app(encodingDesc, kProgramName);
       
        meiHead->addChild(titleStmt);
        meiHead->addChild(respStmt);
        meiHead->addChild(pubStmt);
        meiHead->addChild(sourceDesc);
        meiHead->addChild(encodingDesc);
    }
    return meiHead;
}

/* Create the MEI music element. */
mei::Music* create_music(mei::Music *music) { // TODO dummy function for now
    if (music) {
        mei::Body *body = new mei::Body();
        music->addChild(body);

        mei::Mdiv *mdiv = new mei::Mdiv();
        body->addChild(mdiv);

        mei::Score *score = new mei::Score();
        mdiv->addChild(score);

        mei::ScoreDef *scoreDef = new mei::ScoreDef();
        score->addChild(scoreDef);
    }
    return music;
}

/* MAIN */
int main(int argc, char **argv) {
    // Check input argument
    if (argc != 3) {
    std::cerr << "Usage: mei_header INFILE.ly OUTFILE.xml" << std::endl;
        exit(EXIT_FAILURE);
    }
    // Read input
    std::string infile_str = "";
    std::string infile_name = argv[1];
    std::string outfile_name = argv[2];
    infile_str = file_to_string(infile_name);

    // Parse header 
    dictionary header_dict = extract_header_data(infile_str);

    // Create MEI doc
    mei::MeiDocument *doc = new mei::MeiDocument();
    mei::Mei *mei = new mei::Mei();
    doc->setRootElement(mei);
   
    // Create MEI header
    mei::MeiHead *meiHead = new mei::MeiHead();
    meiHead = create_header(meiHead, header_dict);
    mei->addChild(meiHead);

    // Create MEI music
    mei::Music *music = new mei::Music();
    music = create_music(music);
    mei->addChild(music);

    // Print output
    bool status = mei::documentToFile(doc, outfile_name);
    if (!status) {
        std::cerr << "Error: Could not write output" << std::endl;
        exit(EXIT_FAILURE);
    }

    // Clean up
    delete doc;
}
