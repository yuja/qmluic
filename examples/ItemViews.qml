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
            horizontalHeader.showSortIndicator: true
            verticalHeader.visible: false
        }
    }
}
