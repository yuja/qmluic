#include <QFile>
#include <QtDebug>
#include <cstdint>
#include "pipeserver.h"

PipeServer::PipeServer(QObject *parent) : QThread(parent) { }

void PipeServer::run()
{
    // AFAIK, QFile basically implements blocking interface.
    QFile fin, fout;
    if (!fin.open(0, QIODevice::ReadOnly | QIODevice::Unbuffered)) {
        qWarning() << "failed to open stdin";
        return;
    }
    if (!fout.open(1, QIODevice::WriteOnly | QIODevice::Unbuffered)) {
        qWarning() << "failed to open stdout";
        return;
    }

    union {
        char d[4];
        int32_t n;
    } msg;

    // teach that the viewer process is ready
    msg.n = 0;
    fout.write(msg.d, sizeof(msg));

    // --> {size, payload}
    // <-- {size} (denoting that the data is processed)
    for (;;) {
        if (fin.read(msg.d, sizeof(msg)) != sizeof(msg))
            return; // EOF
        int32_t payloadSize = msg.n;
        if (payloadSize < 0) {
            qWarning() << "invalid payload size:" << payloadSize;
            continue;
        }

        const auto data = fin.read(payloadSize);
        if (data.size() != payloadSize)
            return; // EOF
        msg.n = data.size();
        fout.write(msg.d, sizeof(msg));

        qInfo() << "received payload of" << data.size() << "bytes";
        emit dataReceived(data);
    }
}
