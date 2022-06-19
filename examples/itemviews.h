#pragma once

#include <QWidget>
#include <memory>

namespace Ui {
class ItemViews;
}

class ItemViews : public QWidget
{
    Q_OBJECT

public:
    ItemViews(QWidget *parent = nullptr);
    ~ItemViews() override;

private:
    std::unique_ptr<Ui::ItemViews> ui_;
};
