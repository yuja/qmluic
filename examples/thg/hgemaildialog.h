#pragma once

#include <QDialog>
#include <memory>

namespace Ui {
class HgEmailDialog;
}

class HgEmailDialog : public QDialog
{
    Q_OBJECT

public:
    HgEmailDialog(QWidget *parent = nullptr);
    ~HgEmailDialog() override;

private:
    std::unique_ptr<Ui::HgEmailDialog> ui_;
};
