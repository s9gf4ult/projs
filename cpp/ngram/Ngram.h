#ifndef NGRAM_H
#define NGRAM_H

#include <QHash>
#include <QString>

class Ngram
{
public:
    struct IntString{
        int a;
        int b;
        QString val;
    };
    Ngram();
    QHash<QString, int> getNgram() const;
    void generateNgram(QString data, int nglen);
    QString generateText(int textlen);
    void appendString(QString data);
    QString getRandomText();
    void recalculate();

private:
    QHash<QString, int> ngram;
    QList<IntString> fortext;
    int fortextlen;
    bool recalculated;
};

#endif // NGRAM_H
