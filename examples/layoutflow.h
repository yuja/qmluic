#pragma once

#include <QWidget>
#include <memory>

namespace Ui {
class LayoutFlow;
}

namespace UiSupport {
class LayoutFlow;
}

class LayoutFlow : public QWidget
{
    Q_OBJECT

public:
    explicit LayoutFlow(QWidget *parent = nullptr);
    ~LayoutFlow() override;

private:
    std::unique_ptr<Ui::LayoutFlow> ui_;
    std::unique_ptr<UiSupport::LayoutFlow> uiSupport_;
};
