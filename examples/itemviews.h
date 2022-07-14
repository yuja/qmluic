#pragma once

#include <QWidget>
#include <memory>

class QFileSystemModel;

namespace Ui {
class ItemViews;
}

class ItemViews : public QWidget
{
    Q_OBJECT

public:
    explicit ItemViews(QWidget *parent = nullptr);
    ~ItemViews() override;

private:
    std::unique_ptr<Ui::ItemViews> ui_;
    std::unique_ptr<QFileSystemModel> fsModel_;
};
