#include "uiviewerdialog.h"
#include "ui_uiviewerdialog.h"

UiViewerDialog::UiViewerDialog(QWidget *parent)
    : QDialog(parent), ui_(std::make_unique<Ui::UiViewerDialog>())
{
    ui_->setupUi(this);
}

UiViewerDialog::~UiViewerDialog() = default;

void UiViewerDialog::setContentWidget(std::unique_ptr<QWidget> widget)
{
    // TODO: auto vs fixed layout
    // TODO: if w were window?
    contentWidget_ = std::move(widget);
    ui_->mainLayout->addWidget(contentWidget_.get());
}
