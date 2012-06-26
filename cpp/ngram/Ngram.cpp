#include "Ngram.h"
#include <QFile>
#include <QBuffer>
#include <QtAlgorithms>

Ngram::Ngram() :
    fortextlen(0),
    recalculated(false)
{
}

QHash<QString, int> Ngram::getNgram() const
{
    return ngram;
}

void Ngram::generateNgram(QString data, int nglen)
{
    int datalen = data.length();
    if (datalen >= nglen && nglen > 0) {
        for (int i = 0; i <= datalen - nglen; ++i) {
            appendString(data.mid(i, nglen));
        }
    }
}

QString Ngram::generateText(int textlen)
{
    QString ret;
    if (textlen > 0) {
        for (int i = 0; i <= textlen; ++i) {
            ret += getRandomText();
        }
    }
    return ret;
}

void Ngram::appendString(QString data)
{
    if (ngram.contains(data)) {
        ngram[data] += 1;
    } else {
        ngram.insert(data, 1);
    }
}

QString Ngram::getRandomText()
{
    if (! recalculated) {
        recalculate();
    }
    int r = qrand() % fortextlen;
    foreach(IntString i, fortext) {
        if (r >= i.a && r <= i.b) {
            return i.val;
        }
    }
    return "";
}

void Ngram::recalculate()
{
    fortext.clear();
    int begin = 0;
    QHash<QString, int>::const_iterator iter;
    for (iter = ngram.begin(); iter != ngram.end(); ++iter) {
        IntString x;
        x.a = begin;
        x.b = begin + iter.value() - 1;
        x.val = iter.key();
        begin = x.b + 1;
        fortext.append(x);
    }
    this->recalculated = true;
    this->fortextlen = begin;
}

