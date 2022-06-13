#include <QCloseEvent>
#include <QEvent>
#include <QKeyEvent>
#include <QKeySequence>
#include <QMessageBox>
#include <QMetaObject>
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

void UiViewerDialog::closeEvent(QCloseEvent *event)
{
    if (closable_)
        return;
    QMessageBox::information(this, {},
                             tr("Cannot close viewer while pipe server is running.\n\n"
                                "Terminate the preview command instead."));
    event->ignore();
}

void UiViewerDialog::keyPressEvent(QKeyEvent *event)
{
    if (event->matches(QKeySequence::Cancel)) {
        event->ignore();
        return; // do not close by escape key
    }
    QDialog::keyPressEvent(event);
}

void UiViewerDialog::setClosable(bool closable)
{
    closable_ = closable;
}

void UiViewerDialog::setContentWidget(std::unique_ptr<QWidget> widget)
{
    contentWidget_ = std::move(widget);
    contentWidget_->installEventFilter(this);
    contentWindowFlags_ = contentWidget_->windowFlags(); // backup
    setWindowTitle(contentWidget_->windowTitle().isEmpty()
                           ? tr("UI Viewer")
                           : tr("%1 - UI Viewer").arg(contentWidget_->windowTitle()));
    reparentContentWidget();
}

bool UiViewerDialog::eventFilter(QObject *watched, QEvent *event)
{
    if (watched != contentWidget_.get())
        return QDialog::eventFilter(watched, event);
    switch (event->type()) {
    case QEvent::Hide:
        if (event->spontaneous())
            return false;
        // move widget back to content area on close
        ui_->separateWindowCheck->setChecked(false);
        QMetaObject::invokeMethod(this, &UiViewerDialog::reparentContentWidget,
                                  Qt::QueuedConnection);
        return false;
    default:
        return false;
    }
}

void UiViewerDialog::reparentContentWidget()
{
    if (!contentWidget_)
        return;
    contentWidget_->removeEventFilter(this); // widget will temporarily get hidden
    if (ui_->separateWindowCheck->isChecked()) {
        ui_->contentLayout->removeWidget(contentWidget_.get());
        contentWidget_->setWindowFlags(contentWindowFlags_ | Qt::Window);
    } else {
        contentWidget_->setWindowFlags(contentWindowFlags_ & ~Qt::Window);
        ui_->contentLayout->addWidget(contentWidget_.get());
    }
    contentWidget_->show();
    contentWidget_->installEventFilter(this);
}
