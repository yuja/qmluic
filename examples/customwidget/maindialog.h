#pragma once

#include <QDialog>
#include <memory>

namespace Ui {
class MainDialog;
}

class MainDialog : public QDialog
{
    Q_OBJECT

public:
    MainDialog(QWidget *parent = nullptr);
    ~MainDialog() override;

private:
    std::unique_ptr<Ui::MainDialog> ui_;
};
