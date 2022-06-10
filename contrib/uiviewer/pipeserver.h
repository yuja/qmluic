#pragma once

#include <QByteArray>
#include <QThread>

/// Thread to receive data via stdio.
///
/// This is blocking thread since stdin can't be polled via QFile. The thread will
/// terminate when stdin is closed by peer.
class PipeServer : public QThread
{
    Q_OBJECT

public:
    explicit PipeServer(QObject *parent = nullptr);

protected:
    void run() override;

signals:
    void dataReceived(const QByteArray &data);
};
