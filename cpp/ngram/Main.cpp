
#include "Ngram.h"
#include <QObject>
#include <QString>
#include "stdio.h"
#include <QDebug>
#include <QTextStream>
#include <QTextCodec>
#include <QFile>
#include <QTime>

class Bla : public QObject {
    Q_OBJECT
private:
    QString one;
    QString two;
    QString three;

};

int main(int argc, char ** argv) {
    qsrand(QTime::currentTime().msec());
    QTextStream out(stdout);
    out.setCodec(QTextCodec::codecForName("UTF-8"));
    if (argc != 4) {
        out << "need 3 arguments\n";
        return 1;
    }
    QFile in(argv[1]);
    if (! in.open(in.ReadOnly | in.Text)) {
        out << "can not open the file\n";
        return 1;
    }
    QTextStream instream(&in);
    instream.setCodec(QTextCodec::codecForName("UTF-8"));
    QString data = instream.readAll();
    Ngram ng;
    QString nglenst = argv[2];
    QString textlenst = argv[3];
    ng.generateNgram(data, nglenst.toInt());
    out << ng.generateText(textlenst.toInt());
    out.flush();
}
