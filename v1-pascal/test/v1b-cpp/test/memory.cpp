#include <mei/mei.h>
#include <mei/shared.h>

int main(){
    mei::MeiDocument *doc = new mei::MeiDocument();
    mei::Mei *mei = new mei::Mei();
    doc->setRootElement(mei);
    delete doc;
}
