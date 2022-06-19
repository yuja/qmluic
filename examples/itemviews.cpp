#include "itemviews.h"
#include "ui_itemviews.h"

ItemViews::ItemViews(QWidget *parent) : QWidget(parent), ui_(std::make_unique<Ui::ItemViews>())
{
    ui_->setupUi(this);
}

ItemViews::~ItemViews() = default;
