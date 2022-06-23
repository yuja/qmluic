import qmluic.QtWidgets

QWidget {
    QVBoxLayout {
        QTreeView {
            id: treeView
            header.minimumSectionSize: 100
        }

        QTableView {
            id: tableView
            horizontalHeader.stretchLastSection: true
            // Note that this doesn't enable sorting, but just shows the sorting indicator.
            horizontalHeader.showSortIndicator: true
            verticalHeader.visible: false
        }
    }
}
