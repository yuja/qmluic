#pragma once

#include <QDialogButtonBox>
#include <memory>

namespace Ui {
class MyDialogButtonBox;
}

class MyDialogButtonBox : public QDialogButtonBox
{
    Q_OBJECT

public:
    MyDialogButtonBox(QWidget *parent = nullptr);
    ~MyDialogButtonBox() override;

private:
    std::unique_ptr<Ui::MyDialogButtonBox> ui_;
};
