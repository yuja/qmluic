#pragma once

#include <QWidget>
#include <memory>

namespace Ui {
class StaticItemModel;
}

namespace UiSupport {
class StaticItemModel;
}

class StaticItemModel : public QWidget
{
    Q_OBJECT

public:
    explicit StaticItemModel(QWidget *parent = nullptr);
    ~StaticItemModel() override;

private:
    std::unique_ptr<Ui::StaticItemModel> ui_;
    std::unique_ptr<UiSupport::StaticItemModel> uiSupport_;
};
