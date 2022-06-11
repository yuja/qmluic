#include "uiviewerdialog.h"
#include "ui_uiviewerdialog.h"

UiViewerDialog::UiViewerDialog(QWidget *parent)
    : QDialog(parent), ui_(std::make_unique<Ui::UiViewerDialog>())
{
    ui_->setupUi(this);
    connect(ui_->separateWindowCheck, &QCheckBox::clicked, this,
            &UiViewerDialog::reparentContentWidget);
}

UiViewerDialog::~UiViewerDialog() = default;

void UiViewerDialog::setContentWidget(std::unique_ptr<QWidget> widget)
{
    contentWidget_ = std::move(widget);
    contentWindowFlags_ = contentWidget_->windowFlags(); // backup
    setWindowTitle(contentWidget_->windowTitle().isEmpty()
                           ? tr("UI Viewer")
                           : tr("%1 - UI Viewer").arg(contentWidget_->windowTitle()));
    reparentContentWidget();
}

void UiViewerDialog::reparentContentWidget()
{
    if (!contentWidget_)
        return;
    if (ui_->separateWindowCheck->isChecked()) {
        ui_->contentLayout->removeWidget(contentWidget_.get());
        contentWidget_->setWindowFlags(contentWindowFlags_ | Qt::Window);
        contentWidget_->show();
    } else {
        contentWidget_->setWindowFlags(contentWindowFlags_ & ~Qt::Window);
        ui_->contentLayout->addWidget(contentWidget_.get());
    }
}
