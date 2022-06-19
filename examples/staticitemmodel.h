#pragma once

#include <QWidget>
#include <memory>

namespace Ui {
class StaticItemModel;
}

class StaticItemModel : public QWidget
{
    Q_OBJECT

public:
    StaticItemModel(QWidget *parent = nullptr);
    ~StaticItemModel() override;

private:
    std::unique_ptr<Ui::StaticItemModel> ui_;
};
