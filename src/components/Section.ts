
export default function renderSection(title: string, sectionBody: string, subtitle?: string, code?: string) {
  return /* html */` 
    <div>
      <style scoped>
        .section {
          background-color: var(--vscode-sideBar-background);
          padding: 10px 18px;
          margin: 0px 10px;
        }
        .section-header {
          margin: 10px 0px;
        }
        .header-title {
          color: var(--vscode-foreground);
          font-variant: small-caps;
          font-weight: bolder;
          font-size: 14px;
        }
        .header-subtitle {
          color: #959595;
          font-size: 12px;
        }
        .header-code {
          color: #959595;
          font-weight: bold;
          float: right;
          font-size: 12px;
        }
        .section-body {
          background-color: var(--vscode-editor-background);
          color: var(--vscode-editor-foreground);
          font-size: 12px;
          padding: 10px 12px;
          font-family: monospace;
        }
      </style>

      <div class="section">
        <div class="section-header">
          <span>
            <span class="header-title">${title}</span>
            ${subtitle? /*html*/`
              <span class="header-subtitle">at ${subtitle}</span>`: ""
            }
            ${code && /*html*/`<span class="header-code">#${code}</span>`}
          </span>
        </div>
        ${sectionBody && /*html*/`
          <div class="section-body">${sectionBody}</div>
        `}
      </div>
    </div>
  `
}