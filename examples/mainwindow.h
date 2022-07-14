#pragma once

#include <QMainWindow>
#include <memory>

namespace Ui {
class MainWindow;
}

namespace UiSupport {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow() override;

private slots:
    void updateSourceEdit();

private:
    std::unique_ptr<Ui::MainWindow> ui_;
    std::unique_ptr<UiSupport::MainWindow> uiSupport_;
};
