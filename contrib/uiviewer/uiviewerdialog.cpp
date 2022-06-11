#include "uiviewerdialog.h"
#include "ui_uiviewerdialog.h"

UiViewerDialog::UiViewerDialog(QWidget *parent)
    : QDialog(parent), ui_(std::make_unique<Ui::UiViewerDialog>())
{
    ui_->setupUi(this);
}

UiViewerDialog::~UiViewerDialog() = default;
