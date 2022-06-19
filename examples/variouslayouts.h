#pragma once

#include <QWidget>
#include <memory>

namespace Ui {
class VariousLayouts;
}

class VariousLayouts : public QWidget
{
    Q_OBJECT

public:
    VariousLayouts(QWidget *parent = nullptr);
    ~VariousLayouts() override;

private:
    std::unique_ptr<Ui::VariousLayouts> ui_;
};
